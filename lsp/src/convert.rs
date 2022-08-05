use crate::{LineMap, StateSnapshot, Vfs, VfsPath};
use lsp_types::{Location, Position, Range, TextDocumentPositionParams};
use nil::{FilePos, InFile};
use text_size::TextRange;

pub(crate) fn from_file_pos(
    snap: &StateSnapshot,
    params: &TextDocumentPositionParams,
) -> Option<FilePos> {
    let path = VfsPath::try_from(&params.text_document.uri).ok()?;
    let vfs = snap.vfs.read().unwrap();
    let (file, line_map) = vfs.get(&path)?;
    let pos = line_map.pos(params.position.line, params.position.character);
    Some(FilePos::new(file, pos))
}

pub(crate) fn to_location(vfs: &Vfs, frange: InFile<TextRange>) -> Option<Location> {
    let url = vfs.file_path(frange.file_id)?.try_into().ok()?;
    let line_map = vfs.file_line_map(frange.file_id)?;
    Some(Location::new(url, to_range(line_map, frange.value)))
}

pub(crate) fn to_range(line_map: &LineMap, range: TextRange) -> Range {
    let (line1, col1) = line_map.line_col(range.start());
    let (line2, col2) = line_map.line_col(range.end());
    Range::new(Position::new(line1, col1), Position::new(line2, col2))
}
