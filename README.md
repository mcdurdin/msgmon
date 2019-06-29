# msgmon

Windows user message monitor in the style of procmon.

# Prerequisites

* Windows 10 SDK
* Visual Studio 2017
* Delphi 10.2
* git for Windows

# Building

1. From a git bash prompt, in the root of the repo, run `./configure.sh`. This configures Windows SDK versions
2. From the `source/` folder, run `./build.sh` (optionally, with `-debug` parameter).

# TODO

3. User Interface Improvements
  - save-to-file
  - load-from-file

  - Add window tree view pane
  - Add detail pane
  - time columns
  - basic column choices (plus load and save)
  - hwnd needs to show handle as well as classname
  - add icon
  - sort column classes in MsgMon.System.Data.Column more sensibly
  - suggestions for filter values
  - When saving a context, include session. However, session can also be saved separately.
  - Allow separate hwnd-int, classname, realclassname columns for each hwnd referenced (e.g. in WM_SETFOCUS wParam)
  - Drilling into hwnd
  - multi-line copy and paste
  - bookmarks
  - multiple window view
  - Filtering
    - Save filter to file by default
    - Add filtering on window 'type' - e.g. child, popup, overlapped, topmost, transparent, etc?
    - Add filtering on window visibility, enabled state
    - Add row highlight filters
    - Filter dialog needs row-select on list view
    - Reset filter button in dialog doesn't work
  - Link to MSDN on WM_*?
  - Highlight window in window tree when clicking on hwnd

4. Trace comparisons

5. Provide data needed for call stacks
  - Add events for process start / stop, thread start / stop (using ETW kernel events)
  - http://www.rioki.org/2017/01/09/windows_stacktrace.html (analyzing the stack)
  - https://www.itprotoday.com/microsoft-visual-studio/profiling-and-stack-tracing-event-tracing-windows (collecting the stack)
  - https://blogs.windows.com/buildingapps/2019/05/09/announcing-traceprocessor-preview-0-1-0/#5zwb6AJffOv0z1vz.97 (an alternative, alpha code though)

6. Bugs and issues
  - Some messages have only partial data. Why?

7. Bundling and deployment
  - bundle into single executable for deployment (extract to same folder or temp folder on run?)
 
8. What data is redundant in each message? (PID, TID)

# Data Model

 context
   - processes
   - windows
   - messageNames
   - messages
   - filteredMessages
 
 session
   - filter
   - displayColumns
 
