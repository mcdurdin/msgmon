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
3. If you modify the .vcxproj projects, you need to merge the changes in to the .vcxproj.in projects (these exist to allow for differing Windows SDK versions)

# TODO

3. User Interface Improvements
  - time columns
  - add icon
  - suggestions for filter values
  - Drilling into hwnd
  - bookmarks
  - multiple window view
  - Handle button, listbox, etc types
  - Break messages down into smaller scopes (keyboard, mouse, etc)
  - Filtering
    - IsUnicode?
    - Add filtering on window 'type' - e.g. child, popup, overlapped, topmost, transparent, etc?
    - Add filtering on window visibility, enabled state
    - Add row highlight filters
    - Reset filter button in dialog doesn't work
    - Apply button in Filter should be A&pply.
    - Remove filter should fill the parameter controls with the removed data
  - Copy selected rows on Ctrl+C; also right-click shows selected row count off by 1
  - Link to MSDN on WM_*?
  - Highlight window in window tree when clicking on hwnd
  - Start Trace should have an Administrator elevation icon on it if not already running as Admin (and then do the hoops); 
    this is because, according to docs for StartTrace:

      > Only users with administrative privileges, users in the Performance Log Users group, and services running as
      > LocalSystem, LocalService, NetworkService can control event tracing sessions. To grant a restricted user the
      > ability to control trace sessions, add them to the Performance Log Users group. Only users with 
      > administrative privileges and services running as LocalSystem can control an NT Kernel Logger session.

4. Trace comparisons

5. Provide data needed for call stacks
  - Add events for process start / stop, thread start / stop (using ETW kernel events)
  - http://www.rioki.org/2017/01/09/windows_stacktrace.html (analyzing the stack)
  - https://www.itprotoday.com/microsoft-visual-studio/profiling-and-stack-tracing-event-tracing-windows (collecting the stack)
  - https://blogs.windows.com/buildingapps/2019/05/09/announcing-traceprocessor-preview-0-1-0/#5zwb6AJffOv0z1vz.97 (an alternative, alpha code though)

6. Bugs and issues
  - Some messages have only partial data. Why?
  - Add lock table for writing traces
  - Handle reused PID, TID, HWND situations
  - Cleanup unsigned vs signed chaos for TID,PID,HWND etc

7. Bundling and deployment
  - bundle into single executable for deployment (extract to same folder or temp folder on run?)
 
8. What data is redundant in each message? (PID, TID)

9. Allow for command-line based trace recording, so we can avoid having the GUI app for remote use.

***
1. Search is case sensitive
2. Ctrl+F is not working
3. May need more than 4 search params?
4. Window treeview should show state at time of selected message.
5. Window treeview should also use filters for process name, pid, thread.
6. Window treeview should show icons for apps and windows, and a B&W thread icon.
7. This all means that Window treeview should be implemented as a custom draw control rather than a TTreeView which has sucky perf.
8. A change to how the capture works, to correspond with all this. The capture should be recording transforms from an initial window snapshot, with each thread mapped independently.

* For perf reasons, periodically, the whole transformed snapshot should be written out to the database while converting the trace?

* When a window has a parent from another thread, the child window thread should be responsible for documenting the ownership and this needs to be presented appropriately in the treeview.

* Include/Exclude on window handle fails (int vs hex discrepancy?)

9. Add message time to trace view.


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
 
