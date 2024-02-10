import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	Executable
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	console.log("start snupl2lsp");
	// from $PATH
	const snupldPath = 'snupld';
	const snupld: Executable = {
		command: snupldPath,
		args: [],
		options: {},
		transport: TransportKind.stdio,
	};
	const serverOptions: ServerOptions = snupld;

	// // If the extension is launched in debug mode then the debug server options are used
	// // Otherwise the run options are used
	// const serverOptions: ServerOptions = {
	// 	run: { module: serverModule, transport: TransportKind.ipc },
	// 	debug: {
	// 		module: serverModule,
	// 		transport: TransportKind.ipc,
	// 	}
	// };

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for SnuPL2 documents (*.mod)
		documentSelector: [{ scheme: 'file', language: 'snupl2'}],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
		
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'snupl2lsp',
		'SnuPL2 Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
	// listen for logMessage notifications
	client.onNotification("window/logMessage", (message) => {
		console.log(message);
	});
}

export function deactivate(): Thenable<void> | undefined {
	console.log("stop snupl2lsp");
	if (!client) {
		return undefined;
	}
	return client.stop();
}