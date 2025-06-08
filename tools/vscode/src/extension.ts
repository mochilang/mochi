import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
  const disposable = vscode.commands.registerCommand('mochi.helloWorld', () => {
    vscode.window.showInformationMessage('Hello from Mochi!');
  });

  context.subscriptions.push(disposable);
}

export function deactivate() {}
