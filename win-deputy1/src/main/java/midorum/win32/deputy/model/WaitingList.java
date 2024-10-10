package midorum.win32.deputy.model;

import java.util.List;

public class WaitingList {

    private final List<Waiting> list;
    private final long timeout;

    public WaitingList(final List<Waiting> list, final long timeout) {
        this.list = List.copyOf(list); // unmodifiable list
        this.timeout = timeout;
    }

    public WaitingList() {
        this.list = null;
        this.timeout = 0L;
    }

    public static boolean validateListItem(final Waiting waiting) {
        return waiting != null;
    }

    public static boolean validateList(final List<Waiting> waitingList) {
        return waitingList != null && !waitingList.isEmpty();
    }

    public List<Waiting> getList() {
        return list;
    }

    public long getTimeout() {
        return timeout;
    }

    @Override
    public String toString() {
        return "WaitingList{" +
                "list=" + list +
                ", timeout=" + timeout +
                '}';
    }
}
