package midorum.win32.deputy.ui;

import com.midorum.win32api.struct.PointInt;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

class UtilTest {

    @Test
    void stringToPoint() {
        final Optional<PointInt> result = Util.stringToPoint("(1,2)");
        assertTrue(result.isPresent());
        assertEquals(new PointInt(1,2), result.get());
    }

    @Test
    void stringToPoint2() {
        final Optional<PointInt> result = Util.stringToPoint("(533,962)");
        assertTrue(result.isPresent());
        assertEquals(new PointInt(533,962), result.get());
    }

    @Test
    @Disabled
    void printCharacterCodes() {
//        IntStream.range(0, 65536).forEach(value -> System.out.println(value+ ":"+String.format("%04x",value)+":"+Character.toString(value)));
        System.out.println("\u2190");
    }
}