/* Holding ctrl-c and ctrl-\ while Ctrl-G's editor runs (added in this fork,
 * see FORK.md).
 *
 * The terminal sends SIGINT and SIGQUIT to its whole foreground process
 * group, the program as well as the editor; as a shell does, the program
 * lets them go by while the editor has the terminal. This is done here with
 * sigaction, saving and restoring the actions exactly, rather than through
 * the GHC runtime's table of handlers, which a program may have bypassed for
 * its own handler (as ge does) and which would then restore the wrong one.
 * The signals are caught by a handler that does nothing rather than
 * ignored, since an ignored signal stays ignored in the editor that is
 * started meanwhile and a caught one is reset to its default there. */
#include <signal.h>
#include <string.h>

struct haskeline_held_signals {
    struct sigaction interrupt, quit;
};

static void haskeline_let_go(int signal)
{
    (void)signal;
}

unsigned long haskeline_held_signals_size(void)
{
    return sizeof(struct haskeline_held_signals);
}

int haskeline_hold_interrupts(struct haskeline_held_signals *held)
{
    struct sigaction action;
    memset(&action, 0, sizeof action);
    action.sa_handler = haskeline_let_go;
    action.sa_flags = SA_RESTART;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGINT, &action, &held->interrupt) != 0)
        return -1;
    if (sigaction(SIGQUIT, &action, &held->quit) != 0) {
        sigaction(SIGINT, &held->interrupt, NULL);
        return -1;
    }
    return 0;
}

int haskeline_release_interrupts(const struct haskeline_held_signals *held)
{
    int interrupt = sigaction(SIGINT, &held->interrupt, NULL);
    int quit = sigaction(SIGQUIT, &held->quit, NULL);
    return interrupt == 0 && quit == 0 ? 0 : -1;
}
