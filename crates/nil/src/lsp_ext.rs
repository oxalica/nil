use lsp_types::request::Request;

/// https://github.com/microsoft/language-server-protocol/issues/1002
pub enum ParentModule {}

impl Request for ParentModule {
    type Params = lsp_types::TextDocumentPositionParams;
    type Result = Option<Vec<lsp_types::Location>>;
    const METHOD: &'static str = "experimental/parentModule";
}
