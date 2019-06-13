# msgmon
Windows user message monitor in the style of procmon





# TODO

- Refactor main form code to separate event collection, data management, presentation
  - event collection should be separate executable

- Add detail pane
- time columns
- basic column choices (plus load and save)

- save-to-file
- load-from-file
- add icon
- sort column classes in MsgMon.System.Data.Column more sensibly
- right click for filtering
- suggestions for filter values
- add progress dialog for loading data / applying filter while it's slow
- should we use int64 for integer values?
- When saving a context, include session. However, session can also be saved separately.
- Allow separate hwnd-int, classname, realclassname columns for each hwnd referenced

# TODO FOR THE FUTURE:
 - bundle into single executable for deployment (extract to same folder or temp folder on run?)
 - avoid need for wevtutil im (with tracelogging)
 
 context
   - processes
   - windows
   - messageNames
   - messages
   - filteredMessages
 
 session
   - filter
   - displayColumns
 

