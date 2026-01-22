# How to Audit Dependencies

### Introduction

Over time, projects accumulate “Ghost Dependencies”—packages that are
listed in DESCRIPTION but no longer used in the code.

### Workflow

To scan your project, simply point the scanner at your root directory:

\`\`\`r library(staticanalysis)

## Define your project path

my_project \<- “path/to/my/project”

## Run the scan

report \<- scan_dependencies(my_project)

## View Ghost Packages

print(report\$undeclared_ghosts)
