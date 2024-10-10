package midorum.win32.deputy.model;

import java.util.Set;
import java.util.concurrent.TimeUnit;

public record Waiting(Set<Long> states, long timeout) {

    public static final TimeUnit unit = TimeUnit.SECONDS;

    public Waiting {
        if (states == null || states.isEmpty())
            throw new IllegalArgumentException("waiting should have at least one state");
        if (timeout < 0) throw new IllegalArgumentException("timeout cannot be negative");
        states = Set.copyOf(states);
    }

}
