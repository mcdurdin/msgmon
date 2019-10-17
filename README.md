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

Drill into the data with various Views:
  * Message. This is the default and shows a procmon-style message trace.
  * Timeline. This shows the Process/Thread/Window lifecycle with major events highlighted for each window. Grouped by process/thread; view by owner or parent chains or flat. Filtering will remove windows that don't match requirements.

>> state includes: current focus, current active, parent + owner hierarchy, z-order, window size/position, capture, etc. State transforms need to be captured
>> but for purpose of rapid spelunking should probably be a complete snapshot for a given window state change, attached to a specific message. This changes the
>> window table to capture complete state incl. caption, etc. Focus + activation + capture are a single window for the system, but need to investigate how this
>> works per-thread?

>> UX: The window tree should be refactored to be a point-in-time view:
    >> allow either parent or owner hierarchy (by process/thread at top level), or flat list grouped by process/thread
	>> current focus, current active, current capture should all be highlighted
>> Add a lifetime view: a flat per-thread window list with life diagram highlighting events such as:
    >> activation
	>> focus
	>> movement
	>> input event
	>> creation
	>> destruction
	>> caption change [internalgetwindowtext]
	>> etc.

3. User Interface Improvements
  - time columns
  - add icon
  - tooltips for long fields
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

6. Bugs and issues
  - Some messages have only partial data. Why?
  - Add lock table for writing traces
  - Cleanup unsigned vs signed chaos for TID,PID,HWND etc

7. Bundling and deployment
  - bundle into single executable for deployment (extract to same folder or temp folder on run?)

8. What data is redundant in each message? (PID, TID)

9. Make it easy to do command-line based trace recording, so we can avoid having the GUI app for remote use.

***
1. Search is case sensitive
3. May need more than 4 search params?
5. Window treeview should also use filters for process name, pid, thread.
6. Window treeview should show icons for apps and windows, and a B&W thread icon.

9. Add:
   * message time to trace view.
   * InSendMessageEx data
   * IsWindowUnicode data
   * layered window attributes
   * message extra info
   * process dpi aware; process default layout; window layout (WS_?)



# Data Model

 globalContext
  - messageNames

 messageContext
   - processes
   - windows
   - messages
   - filteredMessages

 session
   - filter
   - displayColumns

