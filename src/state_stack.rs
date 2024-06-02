use std::fs::OpenOptions;
use std::io;
use std::num::NonZeroUsize;
use std::path::Path;

use memmap2::{MmapMut, MmapOptions};

pub struct StateStack {
    mmap: MmapMut,
    file: std::fs::File,
    offsets: Vec<usize>,
    capacity: usize,
}

impl StateStack {
    pub fn new<P: AsRef<Path>>(path: P, capacity: usize) -> std::io::Result<Self> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)?;

        file.set_len(capacity as u64)?;

        let mmap = unsafe { MmapOptions::new().map_mut(&file)? };

        Ok(Self {
            mmap,
            file,
            offsets: Vec::new(),
            capacity,
        })
    }

    fn resize(&mut self, new_capacity: usize) -> std::io::Result<()> {
        println!("resizing mmap {} -> {new_capacity}", self.capacity);
        self.file.set_len(new_capacity as u64)?;
        self.mmap = unsafe { MmapOptions::new().map_mut(&self.file)? };
        self.capacity = new_capacity;
        Ok(())
    }

    pub fn push(&mut self, state: &[u8]) -> std::io::Result<()> {
        let current_offset = self.current_offset();
        if current_offset + state.len() > self.capacity {
            // Double the capacity or increase to fit the new state
            let new_capacity = (self.capacity.max(state.len() + current_offset)) * 2;
            self.resize(new_capacity)?;
        }
        self.mmap[current_offset..current_offset + state.len()].copy_from_slice(state);
        self.offsets.push(current_offset + state.len());
        Ok(())
    }

    pub fn pop(&mut self) -> Option<Vec<u8>> {
        if let Some(&offset) = self.offsets.last() {
            let start = if self.offsets.len() > 1 {
                self.offsets[self.offsets.len() - 2]
            } else {
                0
            };
            let state = self.mmap[start..offset].to_vec();
            self.offsets.pop();
            Some(state)
        } else {
            None
        }
    }

    fn current_offset(&self) -> usize {
        *self.offsets.last().unwrap_or(&0)
    }
}

pub struct LazyStateStack<P: AsRef<Path>> {
    path: Option<P>,
    state_stack: Option<StateStack>,
    capacity_scale: NonZeroUsize,
}

impl<P: AsRef<Path>> LazyStateStack<P> {
    pub fn new(path: P, capacity_scale: NonZeroUsize) -> Self {
        Self {
            path: Some(path),
            state_stack: None,
            capacity_scale,
        }
    }

    pub fn push(&mut self, state: &[u8]) -> std::io::Result<()> {
        let Some(state_stack) = self.state_stack.as_mut() else {
            let path = self.path.take().ok_or(io::Error::new(
                io::ErrorKind::Other,
                "Path should be available",
            ))?;
            let capacity = state.len() * self.capacity_scale.get();
            self.state_stack = Some(StateStack::new(path, capacity)?);
            return self.push(state);
        };
        state_stack.push(state)
    }

    pub fn pop(&mut self) -> Option<Vec<u8>> {
        self.state_stack.as_mut()?.pop()
    }

    fn current_offset(&self) -> usize {
        self.state_stack
            .as_ref()
            .map(|s| s.current_offset())
            .unwrap_or(0)
    }
}
