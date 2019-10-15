#pragma once

enum MMEVENTTYPE { MMEVENT_MESSAGE = 0, MMEVENT_WINDOW, MMEVENT_THREAD, MMEVENT_PROCESS /*, MMEVENT_GLOBAL?*/ };

#define SESSION_NAME  TEXT("MsgMon_Session")
//KERNEL_LOGGER_NAME 


//
// Defines the different event types; these are used as table names as well.
// MMEVENT_EVENT is a meta-type -- it has a corresponding table but no corresponding event
//

#define MMEVENTNAME_EVENT    "Event"
#define MMEVENTNAME_IMAGE    "Image" // for stack tracing

#define MMEVENTNAME_MESSAGE  "Message"
#define MMEVENTNAME_WINDOW   "Window"
#define MMEVENTNAME_PROCESS  "Process"
#define MMEVENTNAME_THREAD   "Thread"

#define MMEVENTNAME_EVENT_L    L"Event"
#define MMEVENTNAME_IMAGE_L    L"Image"

#define MMEVENTNAME_MESSAGE_L  L"Message"
#define MMEVENTNAME_WINDOW_L   L"Window"
#define MMEVENTNAME_PROCESS_L  L"Process"
#define MMEVENTNAME_THREAD_L   L"Thread"

