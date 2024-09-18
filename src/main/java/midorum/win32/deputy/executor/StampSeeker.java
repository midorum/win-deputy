package midorum.win32.deputy.executor;

import dma.util.DurationFormatter;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.model.IExecutor;
import midorum.win32.deputy.model.Stamp;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.time.Duration;
import java.util.*;
import java.util.stream.Collectors;

public class StampSeeker {


    private final Logger executorLogger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final Logger stampLogger = LogManager.getLogger("stamp_logger");
    private final BufferedImage screenImage;
    private final Stamp[] stampsToSeek;
    private final int deviation;

    public StampSeeker(final BufferedImage screenImage, final Stamp[] stampsToSeek, final int deviation) {
        this.screenImage = Objects.requireNonNull(screenImage);
        this.stampsToSeek = Objects.requireNonNull(stampsToSeek);
        this.deviation = deviation;
    }

    public StampSeeker(final BufferedImage screenImage, final Stamp stampToSeek, final int deviation) {
        this(screenImage, new Stamp[]{Objects.requireNonNull(stampToSeek)}, deviation);
    }

    public List<Result> perform() {
        final int[] screen = CommonUtil.imageToArray(screenImage);
        final int screenWidth = screenImage.getWidth();
        final int screenHeight = screenImage.getHeight();
        return Arrays.stream(stampsToSeek)
                .map(stamp -> findStampOnScreen(screen, screenWidth, screenHeight, stamp))
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
    }

    public List<Result> performParallel() {
        final int[] screen = CommonUtil.imageToArray(screenImage);
        final int screenWidth = screenImage.getWidth();
        final int screenHeight = screenImage.getHeight();
        return Arrays.stream(stampsToSeek)
                .parallel()
                .map(stamp -> findStampOnScreen(screen, screenWidth, screenHeight, stamp))
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
    }

    private List<Result> findStampOnScreen(final int[] screen, final int screenWidth, final int screenHeight, final Stamp stamp) {
        final int[] stampData = stamp.wholeData();
        final int stampWidth = stamp.location().width();
        final int stampHeight = stamp.location().height();
        return findStampDataOnScreen(screen, screenWidth, screenHeight, stamp, stampData, stampWidth, stampHeight);
    }

    private List<Result> findStampDataOnScreen(final int[] screen,
                                               final int screenWidth,
                                               final int screenHeight,
                                               final Stamp stamp,
                                               final int[] stampData,
                                               final int stampWidth,
                                               final int stampHeight) {
//        saveRegion("screen", screen, screenWidth, screenHeight);
//        saveRegion("stamp", stampData, stampWidth, stampHeight);
        if (stampWidth > screenWidth)
            throw new IllegalArgumentException("Stamp width cannot be greater than whole screen width");
        if (stampHeight > screenHeight)
            throw new IllegalArgumentException("Stamp height cannot be greater than whole screen height");
        final List<Result> resultList = new ArrayList<>();
        final long start = System.nanoTime();
        for (int row = 0; row <= screenHeight - stampHeight; row++) {
            for (int col = 0; col <= screenWidth - stampWidth; col++) {
                final int baseIndex = col + row * screenWidth;
                stamp:
                {
                    for (int sRow = 0; sRow < stampHeight; sRow++) {
                        for (int sCol = 0; sCol < stampWidth; sCol++) {
                            final int sIndex = sCol + sRow * stampWidth;
                            final int index = (baseIndex + sRow * screenWidth + sCol);
                            if (!colorsAreEquals(screen[index], stampData[sIndex])) {
                                break stamp;
                            }
                        }
                    }
                    resultList.add(new Result(stamp, col, row, deviation, System.nanoTime() - start));
                }
            }
        }
        return resultList;
    }

    private boolean colorsAreEquals(int a, int b) {
        if (a == b) return true;
        if (Math.abs((a & 0xFF) - (b & 0xFF)) > deviation) return false;
        if (Math.abs(((a >> 8) & 0xFF) - ((b >> 8) & 0xFF)) > deviation) return false;
        return Math.abs(((a >> 16) & 0xFF) - ((b >> 16) & 0xFF)) <= deviation;
    }

    private static void saveRegion(final String name, final int[] screen, final int screenWidth, final int screenHeight) {
        CommonUtil.saveImage(CommonUtil.arrayToImage(screen, screenWidth, screenHeight),
                "wrong_shots",
                System.currentTimeMillis() + "_" + name);
    }

    public record Result(Stamp stamp, int x, int y, int deviation, long time) {

        public Result(final Stamp stamp, final int x, final int y, int deviation) {
            this(stamp, x, y, deviation, 0);
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            final Result result = (Result) o;
            return x == result.x && y == result.y && Objects.equals(stamp, result.stamp);
        }

        @Override
        public int hashCode() {
            return Objects.hash(stamp, x, y);
        }

        @Override
        public String toString() {
            return "Result{" +
                    "stamp=" + stamp.name() +
                    ", x=" + x +
                    ", y=" + y +
                    ", deviation=" + deviation +
                    ", time=" + new DurationFormatter(Duration.ofNanos(time)).toStringWithoutZeroParts() +
                    '}';
        }
    }
}
