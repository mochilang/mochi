import * as child_process from 'child_process';
import * as vscode from 'vscode';
import { LanguageClient, StreamInfo } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
  const serverOptions = (): Promise<StreamInfo> => {
    const child = child_process.spawn('mochi-lsp', []);
    return Promise.resolve({ reader: child.stdout, writer: child.stdin });
  };

  const clientOptions = { documentSelector: [{ scheme: 'file', language: 'mochi' }] };

  client = new LanguageClient('mochi-language-server', 'Mochi Language Server', serverOptions, clientOptions);
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
