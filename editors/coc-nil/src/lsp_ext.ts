import * as lc from 'coc.nvim';

export const parentModule = new lc.RequestType<lc.TextDocumentPositionParams, lc.Location[] | null, void>(
  'experimental/parentModule'
);
