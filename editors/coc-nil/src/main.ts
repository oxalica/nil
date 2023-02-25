import { ExtensionContext, services, LanguageClient, workspace } from 'coc.nvim';

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
}
