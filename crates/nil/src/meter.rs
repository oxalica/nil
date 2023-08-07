use std::future::Future;
use std::io;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::task::{Context, Poll};
use std::time::Instant;

use async_lsp::{AnyEvent, AnyNotification, AnyRequest, LspService};
use serde::Serialize;
use tower::{Layer, Service};

const LEVEL: tracing::Level = tracing::Level::DEBUG;

pub struct Meter<S> {
    service: S,
}

impl<S: LspService> Service<AnyRequest> for Meter<S>
where
    S::Future: 'static,
    S::Response: Serialize,
    S::Error: Serialize,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + 'static>>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.service.poll_ready(cx)
    }

    fn call(&mut self, req: AnyRequest) -> Self::Future {
        // Fast path.
        if !tracing::event_enabled!(LEVEL) {
            return Box::pin(self.service.call(req));
        }

        let inst = Instant::now();
        let fut = self.service.call(req);
        Box::pin(async move {
            let ret = fut.await;
            let elapsed = inst.elapsed();
            let mut counter = CounterWriter::default();
            match &ret {
                Ok(v) => serde_json::to_writer(&mut counter, v),
                Err(err) => serde_json::to_writer(&mut counter, err),
            }
            .expect("Failed to serialize");
            tracing::event!(LEVEL, "respond with ~{} bytes in {:?}", counter.0, elapsed);
            ret
        })
    }
}

impl<S: LspService> LspService for Meter<S>
where
    S::Future: 'static,
    S::Response: Serialize,
    S::Error: Serialize,
{
    fn notify(&mut self, notif: AnyNotification) -> ControlFlow<async_lsp::Result<()>> {
        let inst = Instant::now();
        let ret = self.service.notify(notif);
        let elapsed = inst.elapsed();
        tracing::event!(LEVEL, "handled notification in {elapsed:?}");
        ret
    }

    fn emit(&mut self, event: AnyEvent) -> ControlFlow<async_lsp::Result<()>> {
        let inst = Instant::now();
        let ret = self.service.emit(event);
        let elapsed = inst.elapsed();
        tracing::event!(LEVEL, "handled event in {elapsed:?}");
        ret
    }
}

#[derive(Debug, Default)]
struct CounterWriter(u64);

impl io::Write for CounterWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0 += buf.len() as u64;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Default)]
pub struct MeterLayer;

impl<S> Layer<S> for MeterLayer {
    type Service = Meter<S>;

    fn layer(&self, inner: S) -> Self::Service {
        Meter { service: inner }
    }
}
