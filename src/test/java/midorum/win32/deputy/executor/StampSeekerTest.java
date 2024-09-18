package midorum.win32.deputy.executor;

import static org.junit.jupiter.api.Assertions.*;

import com.midorum.win32api.facade.Rectangle;
import dma.util.TimeMeasurer2;
import midorum.win32.deputy.model.Stamp;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.function.Function;

class StampSeekerTest {

    private static final int DEVIATION = 1;

    @BeforeEach
    void beforeEach() {
    }

    @Test
    void test1Sequential() {
        basicTest("sequential", StampSeeker::perform, getTest1Data());
    }

    @Test
    void test1Parallel() {
        basicTest("parallel", StampSeeker::performParallel, getTest1Data());
    }

    @Test
    void test2Sequential() {
        basicTest("sequential", StampSeeker::perform, getTest2Data());
    }

    @Test
    void test2Parallel() {
        basicTest("parallel", StampSeeker::performParallel, getTest2Data());
    }

    @Test
    void test3Sequential() {
        basicTest("sequential", StampSeeker::perform, getTest3Data());
    }

    @Test
    void test3Parallel() {
        basicTest("parallel", StampSeeker::performParallel, getTest3Data());
    }

    @Test
    void testBenchmark() {
        basicBenchmark("sequential", StampSeeker::perform, getTest1Data());
        basicBenchmark("parallel", StampSeeker::performParallel, getTest1Data());
        basicBenchmark("sequential", StampSeeker::perform, getTest2Data());
        basicBenchmark("parallel", StampSeeker::performParallel, getTest2Data());
        basicBenchmark("sequential", StampSeeker::perform, getTest3Data());
        basicBenchmark("parallel", StampSeeker::performParallel, getTest3Data());
    }

    private void basicTest(final String mode, final Function<StampSeeker, List<StampSeeker.Result>> action, final TestData testData) {
        final StampSeeker stampSeeker = new StampSeeker(testData.screenImage, testData.stampsToSeek, DEVIATION);
        final TimeMeasurer2.MeasurementResult<List<StampSeeker.Result>> measurementResult = TimeMeasurer2.execute(testData.name + "(" + mode + ")", () -> action.apply(stampSeeker));
        final List<StampSeeker.Result> resultList = measurementResult.result().orElseThrow();
        verifyResult(testData.shouldBeResult, resultList);
        System.out.println(measurementResult);
        measurementResult.result().ifPresent(results -> results.forEach(System.out::println));
    }

    private void basicBenchmark(final String mode, final Function<StampSeeker, List<StampSeeker.Result>> action, final TestData testData) {
        final StampSeeker stampSeeker = new StampSeeker(testData.screenImage, testData.stampsToSeek, DEVIATION);
        System.out.println("Warmup...");
        for (int i = 0; i < 10; i++) {
            action.apply(stampSeeker);
        }
        System.out.println("Benchmark...");
        final TimeMeasurer2.MeasurementResult<List<StampSeeker.Result>> measurementResult = TimeMeasurer2.execute(testData.name + "(" + mode + ")", () -> action.apply(stampSeeker));
        final List<StampSeeker.Result> resultList = measurementResult.result().orElseThrow();
        verifyResult(testData.shouldBeResult, resultList);
        System.out.println(measurementResult);
        measurementResult.result().ifPresent(results -> results.forEach(System.out::println));
    }

    private static void verifyResult(final List<StampSeeker.Result> shouldBeResult, final List<StampSeeker.Result> resultList) {
        assertNotNull(resultList);
        assertEquals(shouldBeResult.size(), resultList.size(), "result list size should be " + shouldBeResult.size() + " but is " + resultList.size() + " (" + resultList + ")");
        assertTrue(resultList.containsAll(shouldBeResult), "result list should contain all of " + shouldBeResult + " but is " + resultList);
        assertTrue(shouldBeResult.containsAll(resultList), "result list shouldn't contain any except of " + shouldBeResult + " but is " + resultList);
    }

    private BufferedImage generateImage(final Rectangle rectangle) {
        final int width = rectangle.width();
        final int height = rectangle.height();
        BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2d = img.createGraphics();
        g2d.setColor(Color.BLACK);
        g2d.fillRect(0, 0, width, height);
        g2d.setColor(Color.RED);
        g2d.drawLine(0, 0, width, height);
        g2d.drawLine(0, height, width, 0);
        g2d.dispose();
        return img;
    }

    private BufferedImage generateImage(final Rectangle rectangle, final TextLabel... labels) {
        final int width = rectangle.width();
        final int height = rectangle.height();
        BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2d = img.createGraphics();
        g2d.setColor(Color.BLACK);
        g2d.fillRect(0, 0, width, height);
        for (final TextLabel label : labels) {
            g2d.setColor(label.color);
            g2d.drawString(label.text, label.x, label.y);
        }
        g2d.dispose();
        return img;
    }

    private BufferedImage createImage(final int[] data, final int width) {
        if (data.length % width != 0)
            throw new IllegalArgumentException("the array must contain an entire number of rows");
        final int height = data.length / width;
        final BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        image.setRGB(0, 0, width, height, data, 0, width);
        return image;
    }

    private Stamp createStamp(final String name, final Rectangle location, final int[] wholeData) {
        if (wholeData.length % location.width() != 0)
            throw new IllegalArgumentException("the array must contain an entire number of rows (array length=" + wholeData.length + ", rectangle width=" + location.width() + ")");
        return new Stamp(name, wholeData, location);
    }

    private Stamp createStampAsSubImage(final String name, final BufferedImage screenImage, final int left, final int top, final int width, final int height) {
        return createStamp(name, new Rectangle(left, top, left + width, top + height), imageToArray(screenImage.getSubimage(left, top, width, height)));
    }

    private Stamp createStampFromImage(final String name, final BufferedImage stampImage) {
        return createStamp(name, new Rectangle(0, 0, stampImage.getWidth(), stampImage.getHeight()), imageToArray(stampImage));
    }

    private int[] imageToArray(final BufferedImage image) {
        return image.getRGB(
                image.getMinX(),
                image.getMinY(),
                image.getWidth(),
                image.getHeight(),
                null,
                0,
                image.getWidth());
    }

    private record TextLabel(String text, int x, int y, Color color) {


    }

    private TestData getTest1Data() {
        final BufferedImage screenImage = createImage(new int[]{
                0, 1, 0, 1, 0,
                0, 1, 0, 1, 0,
                0, 1, 2, 2, 0,
                0, 1, 1, 2, 0,
                0, 1, 0, 1, 0,
                0, 1, 0, 2, 1,
                0, 1, 1, 2, 2}, 5);
        final Stamp stamp = createStamp("test1", new Rectangle(0, 0, 2, 2), new int[]{
                2, 2,
                2, 3});
        final Stamp[] stampsToSeek = {stamp};
        final List<StampSeeker.Result> shouldBeResult = List.of(new StampSeeker.Result(stamp, 2, 2, DEVIATION),
                new StampSeeker.Result(stamp, 3, 5, DEVIATION));
        return new TestData("test1", screenImage, stampsToSeek, shouldBeResult);
    }

    private TestData getTest2Data() {
        final BufferedImage screenImage = generateImage(new Rectangle(0, 0, 1000, 1000));
        final Stamp stamp = createStampAsSubImage("test2", screenImage, 490, 490, 100, 100);
        final Stamp[] stampsToSeek = {stamp};
        final List<StampSeeker.Result> shouldBeResult = List.of(new StampSeeker.Result(stamp, 490, 490, DEVIATION));
        return new TestData("test2", screenImage, stampsToSeek, shouldBeResult);
    }

    private TestData getTest3Data() {
        final BufferedImage screenImage = generateImage(new Rectangle(0, 0, 2000, 2000),
                new TextLabel("test", 245, 178, Color.BLUE),
                new TextLabel("test", 34, 12, Color.RED),
                new TextLabel("test", 320, 98, Color.BLUE),
                new TextLabel("873509", 800, 723, Color.CYAN));
        final Stamp stamp1 = createStampFromImage("test31",
                generateImage(new Rectangle(0, 0, 50, 15), new TextLabel("test", 0, 10, Color.BLUE)));
        final Stamp stamp2 = createStampFromImage("test32",
                generateImage(new Rectangle(0, 0, 57, 20), new TextLabel("test", 0, 10, Color.RED)));
        final Stamp stamp3 = createStampFromImage("test33",
                generateImage(new Rectangle(0, 0, 100, 23), new TextLabel("873509", 0, 10, Color.CYAN)));
        final Stamp stamp4 = createStampFromImage("test34",
                generateImage(new Rectangle(0, 0, 75, 21), new TextLabel("error", 0, 10, Color.DARK_GRAY)));
        final Stamp[] stampsToSeek = {stamp1, stamp2, stamp3, stamp4};
        final List<StampSeeker.Result> shouldBeResult = List.of(
                new StampSeeker.Result(stamp1, 245, 168, DEVIATION),
                new StampSeeker.Result(stamp1, 320, 88, DEVIATION),
                new StampSeeker.Result(stamp2, 34, 2, DEVIATION),
                new StampSeeker.Result(stamp3, 800, 713, DEVIATION));
        return new TestData("test3", screenImage, stampsToSeek, shouldBeResult);
    }

    private record TestData(String name, BufferedImage screenImage, Stamp[] stampsToSeek,
                            List<StampSeeker.Result> shouldBeResult) {

    }

    public static void main(String[] args) {
        final Visualizer visualizer = new StampSeekerTest().new Visualizer();
//        visualizer.x1();
        visualizer.x2();
    }

    private class Visualizer {

        private void x1() {
            final BufferedImage screenImage = generateImage(new Rectangle(0, 0, 1000, 1000));
            final BufferedImage subImage = screenImage.getSubimage(490, 490, 100, 100);
            showImage("screen", screenImage);
            showImage("subImage", subImage);
        }

        private void x2() {
            final BufferedImage screenImage = generateImage(new Rectangle(0, 0, 2000, 2000),
                    new TextLabel("test", 245, 178, Color.BLUE),
                    new TextLabel("test", 34, 12, Color.RED),
                    new TextLabel("test", 320, 98, Color.BLUE),
                    new TextLabel("873509", 800, 723, Color.CYAN));
            final BufferedImage stampImage = generateImage(new Rectangle(0, 0, 50, 15), new TextLabel("test", 0, 10, Color.BLUE));
            showImage("screen", screenImage);
            showImage("stamp", stampImage);
        }

        public void showImage(final String title, final BufferedImage image) {
            JDialog dialog = new JDialog();
            dialog.setTitle(title);
            dialog.setPreferredSize(new Dimension(image.getWidth() + 10, image.getHeight() + 30));
            dialog.add(new JPanel() {
                @Override
                public void paint(final Graphics g) {
                    g.drawImage(image, 0, 0, this);
                }
            });
            dialog.pack();
            dialog.validate();
            dialog.setVisible(true);
        }

    }
}
