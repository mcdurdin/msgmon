# msgmon

Windows user message monitor in the style of procmon

I've already used this successfully to capture a trace of a focus problem in a multi-process app. Yay!

# TODO

1. Refactor main form code to separate event collection, data management, presentation
   (event collection should be separate executable)
  - Add events for process start / stop, thread start / stop (using ETW kernel events)

2. In-memory buffering for data is not workable. A 30 second trace generated a 165MB ETL file, which turned into a 700MB XML, which crashed MsgMon (2GB+ limit)
  - Rewrite backing to use file storage; binary format.
  - Can we use Pagefile backing like Procmon uses?
  - What data is redundant in each message? (PID, TID, )
  - What data can be 32 bit? (HWND, HKL, )
    - should we use int64 for integer values?
  - Procmon seems to use a format internally that is not ETL so is presumably converting ETL events as they come in. Do we want to do something similar?
  - sqlite is probably an appropriate choice
  - read from pipe in a separate thread until trace stop.

3. User Interface Improvements
  - Add window tree view pane
  - Add detail pane
  - time columns
  - basic column choices (plus load and save)
  - hwnd needs to show handle as well as classname
  - save-to-file
  - load-from-file
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
    - Add filtering on window 'type' - e.g. child, popup, overlapped, topmost, transparent, etc?
    - Add filtering on window visibility, enabled state
    - Add row highlight filters
    - Filter dialog needs row-select on list view
  - Link to MSDN on WM_*?
  - Highlight window in window tree when clicking on hwnd

4. Trace comparisons

5. Provide data needed for call stacks
  - http://www.rioki.org/2017/01/09/windows_stacktrace.html (analyzing the stack)
  - https://www.itprotoday.com/microsoft-visual-studio/profiling-and-stack-tracing-event-tracing-windows (collecting the stack)
  - https://blogs.windows.com/buildingapps/2019/05/09/announcing-traceprocessor-preview-0-1-0/#5zwb6AJffOv0z1vz.97 (an alternative, alpha code though)

6. Bugs and issues
  - Some messages have only partial data. Why?

7. Bundling and deployment
  - bundle into single executable for deployment (extract to same folder or temp folder on run?)
 
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
 
