pub trait IsSomeAndSame {
    fn is_some_and_same(&self, other: &Self) -> bool;
}

impl<T> IsSomeAndSame for Option<T>
where
    T: PartialEq,
{
    fn is_some_and_same(&self, other: &Self) -> bool {
        match (self, other) {
            (None, _) | (_, None) => false,
            (Some(a), Some(b)) => a == b,
        }
    }
}

pub mod tracing {
    use std::sync::Once;

    use tracing_subscriber::EnvFilter;
    use tracing_subscriber::fmt::format::FmtSpan;

    static TRACING_SUBSCRIBER: Once = Once::new();

    pub fn init_tracing() {
        TRACING_SUBSCRIBER.call_once(|| {
            tracing_subscriber::fmt()
                .with_env_filter(EnvFilter::from_default_env())
                .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
                .with_target(true)
                .init();
            tracing::info!("Info enabled");
            tracing::debug!("Debug enabled");
            tracing::trace!("Trace enabled");
        })
    }
}
