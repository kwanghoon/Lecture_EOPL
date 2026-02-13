# Rocq (Windows) – Binary Installer Setup

Reference: Rocq Windows binary installer + VS Code guide  
https://rocq-prover.org/install#windows-vscode

## 1) Let Rocq know the binary and library directories

In **PowerShell**, set the following environment variables:

```powershell
$env:COQBIN  = "C:\rocq\bin"
$env:ROCQLIB = "C:\rocq\lib\coq"
```

## 2) Run rocq everywhere (add to PATH)

```powershell
$env:PATH += ";C:\rocq\bin"
```

## 3) Compile *.v

```powershell
roqc comiple Expr.v
roqc compile Env.v
roqc compile Interp.v
roqc compile TyEnv.v
roqc compile TypeCheck.v
roqc compile TypeSound.v
```

## Notes

- The commands above apply to the current PowerShell session.

- To make them permanent, add the same values to Windows Environment Variables (System Properties → Environment Variables).