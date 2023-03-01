import {
  ExtensionContext,
  services,
  LanguageClient,
  workspace,
  commands,
  TextDocumentPositionParams,
  Uri,
  Position,
} from 'coc.nvim';
import * as lsp_ext from './lsp_ext';

const ROOT_SECTION = 'nil';
const LANGUAGE_ID = 'nix';

export async function activate(context: ExtensionContext): Promise<void> {
  const cfg = workspace.getConfiguration(ROOT_SECTION);
  if (!cfg.get('enable', true)) {
    return;
  }

  const serverOptions = {
    command: cfg.get<string>('server.path', 'nil'),
  };
  const clientOptions = {
    documentSelector: [{ language: LANGUAGE_ID }],
  };
  const client = new LanguageClient('nil', 'nil Language Server', serverOptions, clientOptions);
  context.subscriptions.push(services.registLanguageClient(client));

  context.subscriptions.push(commands.registerCommand('nil.fileReferrer', () => fileReferrer(client), null, false));
}

async function fileReferrer(client: LanguageClient) {
  const { document } = await workspace.getCurrentState();

  const position = Position.create(0, 0);
  const param: TextDocumentPositionParams = {
    textDocument: { uri: document.uri },
    position,
  };
  const locations = await client.sendRequest(lsp_ext.parentModule, param);
  if (!locations || locations.length === 0) return;

  if (locations.length === 1) {
    workspace.jumpTo(locations[0].uri, locations[0].range.start);
  } else {
    await commands.executeCommand('editor.action.showReferences', Uri.parse(document.uri), position, locations);
  }
}
