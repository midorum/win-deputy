package midorum.win32.deputy.ui;

import com.midorum.win32api.struct.PointInt;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class UtilTest {

    @Test
    void stringToPoint() {
        final Optional<PointInt> result = Util.stringToPoint("(1,2)");
        assertTrue(result.isPresent());
        assertEquals(new PointInt(1,2), result.get());
    }
}