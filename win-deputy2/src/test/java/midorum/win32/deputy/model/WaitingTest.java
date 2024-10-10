package midorum.win32.deputy.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class WaitingTest {

    @Test
    void testImmutability() {
        final HashSet<Long> states = new HashSet<>();
        states.add(0L);
        final Waiting waiting = new Waiting(states, 0);
        states.add(-1L);
        assertEquals(1, waiting.states().size());
    }

    @Test
    void failIfTimeoutIsNegative() {
        assertThrows(IllegalArgumentException.class, () -> new Waiting(Set.of(0L), -1));
    }

    @Test
    void passIfTimeoutIsNotNegative() {
        final long timeout = 0;
        final Waiting waiting = new Waiting(Set.of(0L), timeout);
        assertEquals(timeout, waiting.timeout());
    }

    @Test
    void testSerialization() throws JsonProcessingException {
        final JsonSerializer<Waiting> serializer = new JsonSerializer<>(Waiting.class);
        final Waiting src = new Waiting(Set.of(0L), 0L);
        final Waiting dst = serializer.fromJson(serializer.toJson(src));
        assertNotNull(dst);
        assertEquals(src.states(), dst.states());
        assertEquals(src.timeout(), dst.timeout());
    }
}