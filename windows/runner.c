#define PROGRAM "PassStorage.exe"

#include <windows.h>

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	STARTUPINFO si;
	memset(&si, 0, sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);
	// si.dwFlags = STARTF_USESHOWWINDOW;
	// si.wShowWindow = SW_HIDE;

	PROCESS_INFORMATION pi;
	return CreateProcess(PROGRAM, NULL, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &si, &pi)
		? 0
		: 1;
}
