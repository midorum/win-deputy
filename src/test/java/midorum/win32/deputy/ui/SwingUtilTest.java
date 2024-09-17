package midorum.win32.deputy.ui;

//import am.ik.yavi.core.ConstraintViolations;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import com.midorum.win32api.struct.PointInt;
import midorum.win32.deputy.model.*;
import midorum.win32.deputy.ui.editor.Util;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class SwingUtilTest {

    @Test
    void stringToPoint() {
        final Optional<PointInt> result = Util.stringToPoint("(1,2)");
        assertTrue(result.isPresent());
        assertEquals(new PointInt(1, 2), result.get());
    }

    @Test
    void stringToPoint2() {
        final Optional<PointInt> result = Util.stringToPoint("(533,962)");
        assertTrue(result.isPresent());
        assertEquals(new PointInt(533, 962), result.get());
    }

    @Test
    @Disabled
    void printCharacterCodes() {
//        IntStream.range(0, 65536).forEach(value -> System.out.println(value+ ":"+String.format("%04x",value)+":"+Character.toString(value)));
        System.out.println("\u2190");
    }

    @Test
    void scenarioToJsonTest() {
        final Scenario scenario = new Scenario();
        System.out.println(scenario);
        final ObjectMapper objectMapper = new ObjectMapper()
                .registerModule(new ParameterNamesModule())
                .registerModule(new Jdk8Module())
                .registerModule(new JavaTimeModule());
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        try {
            final String s = objectMapper.writeValueAsString(scenario);
            System.out.println(s);
            final Scenario parsed = objectMapper.readValue(s, Scenario.class);
            System.out.println(parsed);
        } catch (JsonProcessingException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Test
    void activityToJsonTest() {
        final Activity activity = new Activity();
        System.out.println(activity);
        final ObjectMapper objectMapper = new ObjectMapper()
                .registerModule(new ParameterNamesModule())
                .registerModule(new Jdk8Module())
                .registerModule(new JavaTimeModule());
        try {
            final String s = objectMapper.writeValueAsString(activity);
            System.out.println(s);
            final Activity parsed = objectMapper.readValue(s, Activity.class);
            System.out.println(parsed);
        } catch (JsonProcessingException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Test
    void waitingToJsonTest() {
        final Waiting waiting = new Waiting();
        System.out.println(waiting);
        final ObjectMapper objectMapper = new ObjectMapper();
        try {
            final String s = objectMapper.writeValueAsString(waiting);
            System.out.println(s);
        } catch (JsonProcessingException ex) {
            throw new RuntimeException(ex);
        }
    }

///    @Test
///    void scenarioValidationTest() {
///        final HashMap<CheckDataType, String> checkData = new HashMap<>() {{
///            put(CheckDataType.processName, "test");
///            put(CheckDataType.windowTitle, "invalid");
///        }};
///        final Scenario scenario = new Scenario(
///                "title",
///                null,
///                List.of(new Activity(
///                        "title",
///                        null,
///                        List.of(new Check(CheckType.processExists, checkData)),
///                        List.of(),
///                        null
///                )),
///                ScenarioType.oneTime,
///                Map.of()
///        );
///        final ConstraintViolations violations = Scenario.validator.validate(scenario);
///        System.out.println(violations.isValid());
///        violations.forEach(constraintViolation -> {
///            System.out.println(constraintViolation.message());
///        });
///    }

}