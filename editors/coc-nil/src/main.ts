import { ExtensionContext, services, LanguageClient, workspace, commands } from 'coc.nvim';
import * as lsp_ext from './lsp_ext';

const ROOT_SECTION = 'nil';

export async function activate(context: ExtensionContext): Promise<void> {
  const cfg = workspace.getConfiguration(ROOT_SECTION);
  if (!cfg.get('enable', true)) {
    return;
  }

  const serverOptions = {
    command: cfg.get<string>('server.path', 'nil'),
  };
  const clientOptions = {
    documentSelector: [{ language: 'nix' }],
  };
  const client = new LanguageClient('nil', 'nil Language Server', serverOptions, clientOptions);
  context.subscriptions.push(services.registLanguageClient(client));
  context.subscriptions.push(commands.registerCommand('nil.reloadFlake', () => onReloadFlake(client)));
}

function onReloadFlake(client: LanguageClient) {
  client.sendNotification(lsp_ext.reloadFlake);
}
