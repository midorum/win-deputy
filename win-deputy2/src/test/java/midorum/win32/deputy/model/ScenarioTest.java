package midorum.win32.deputy.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class ScenarioTest {

    @Test
    void testSerialization() throws JsonProcessingException {
        final JsonSerializer<Scenario> serializer = new JsonSerializer<>(Scenario.class);
        final Check check = new Check(-1, CheckType.processExists, Map.of(), true);
        final Command command = new Command(CommandType.keyboardType, -1L, Map.of());
        final Case aCase = new Case(-1, "test aCase", null, List.of(check), List.of(command),
                null, true, true, true);
        final Scenario src = new Scenario("test", null, List.of(aCase), ScenarioType.oneTime, null);
        final Scenario dst = serializer.fromJson(serializer.toJson(src));
        assertNotNull(dst);
        assertEquals(src.title(), dst.title());
        assertEquals(src.description(), dst.description());
        assertEquals(src.cases(), dst.cases());
        assertEquals(src.type(), dst.type());
        assertEquals(src.data(), dst.data());
    }
}