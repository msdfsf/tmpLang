import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

function ensureTasksJsonHasViDebuggerTasks() {

  const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
  if (!workspaceFolder) {
    vscode.window.showErrorMessage("No workspace folder found!");
    return;
  }

  const tasksJsonPath = path.join(workspaceFolder.uri.fsPath, ".vscode", "tasks.json");

  let tasksJson: any = { version: "2.0.0", tasks: [] };

  // Check if tasks.json exists
  if (fs.existsSync(tasksJsonPath)) {
    try {
      tasksJson = JSON.parse(fs.readFileSync(tasksJsonPath, "utf-8"));
    } catch (error) {
      console.error("Error parsing tasks.json:", error);
      vscode.window.showErrorMessage("Error reading tasks.json file.");
      return;
    }
  }

  // Define your Vi Debugger tasks
  const viDebuggerTasks = [
    {
      "label": "Vi Translate and build",
      "type": "shell",
      "group": { "kind": "build", "isDefault": true },
      "dependsOrder": "sequence",
      "dependsOn": ["vi-translate", "vi-build"]
    },
    {
      "label": "vi-translate",
      "type": "shell",
      "command": "${config:viDebugger.compilerPath}/build/compiler.exe translate ${file} -gd",
      "group": { "kind": "build", "isDefault": false },
      "problemMatcher": []
    },
    {
      "label": "vi-build",
      "type": "shell",
      "command": "gcc",
      "args": [
        "-g", "-O0", "-o", "./out/to_debug",
        "-I", "./out", "-I", "${config:viDebugger.compilerPath}/resources",
        "./out/main.c"
      ],
      "group": { "kind": "build", "isDefault": false },
      "problemMatcher": []
    }
  ];

  let updated = false;

  // add missing tasks
  for (const newTask of viDebuggerTasks) {
    if (!tasksJson.tasks.some((task: any) => task.label === newTask.label)) {
      tasksJson.tasks.push(newTask);
      updated = true;
    }
  }

  // write back if updated
  if (updated) {
    fs.mkdirSync(path.dirname(tasksJsonPath), { recursive: true });
    fs.writeFileSync(tasksJsonPath, JSON.stringify(tasksJson, null, 2));
    vscode.window.showInformationMessage("Vi Debugger tasks added to tasks.json!");
  }

}

export function activate(context: vscode.ExtensionContext) {
  
  ensureTasksJsonHasViDebuggerTasks();

  context.subscriptions.push(
    vscode.commands.registerCommand('vi-debugger.ensureTasks', ensureTasksJsonHasViDebuggerTasks)
  );

  // ensure tasks exist when debugging starts
  vscode.debug.onDidStartDebugSession(() => {
    ensureTasksJsonHasViDebuggerTasks();
  });

}
