package midorum.win32.deputy.ui;

import com.midorum.win32api.struct.PointInt;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.i18n.UiElement;

import javax.swing.*;
import java.awt.*;

class CoordinatesInput extends JPanel {

    private final State state;
    private final IntegerTextField xField;
    private final IntegerTextField yField;

    public static CoordinatesInput getForAbsoluteCoordinates(final String value, final State state) {
        final DisplayMode currentDisplayMode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        final int topWidth = currentDisplayMode.getWidth() - 1;
        final int topHeight = currentDisplayMode.getHeight() - 1;
        return new CoordinatesInput(0, topWidth,
                0, topHeight,
                "x (0-" + topWidth + ")", "y (0-" + topHeight + ")",
                value, state);
    }

    public static CoordinatesInput getForAbsoluteCoordinates(final State state) {
        return getForAbsoluteCoordinates(null, state);
    }

    public static CoordinatesInput getForCoordinatesOffset(final String value, final State state) {
        return new CoordinatesInput(Integer.MIN_VALUE, Integer.MAX_VALUE,
                Integer.MIN_VALUE, Integer.MAX_VALUE,
                "x", "y",
                value, state);
    }

    public static CoordinatesInput getForCoordinatesOffset(final State state) {
        return getForCoordinatesOffset(null, state);
    }

    private CoordinatesInput(final int xLowBound, final int xTopBound,
                             final int yLowBound, final int yTopBound,
                             final String xLabel, final String yLabel,
                             final String value, final State state) {
        this.state = state;
        xField = new IntegerTextField(xLowBound, xTopBound);
        yField = new IntegerTextField(yLowBound, yTopBound);
        if (value != null) {
            CommonUtil.stringToPoint(value).ifPresentOrElse(pointInt -> {
                xField.setText(Integer.toString(pointInt.x()));
                yField.setText(Integer.toString(pointInt.y()));
            }, () -> state.getUtilities().reportIllegalState(UiElement.cannotParseCoordinates, value));
        }
        SwingUtil.putComponentsToHorizontalGrid(this,
                new double[]{0.0, 0.5, 0.0, 0.5},
                new JLabel(xLabel),
                xField,
                new JLabel(yLabel),
                yField
        );
    }

    public void setDataValue(final String value) {
        CommonUtil.stringToPoint(value).ifPresentOrElse(pointInt -> {
            xField.setText(Integer.toString(pointInt.x()));
            yField.setText(Integer.toString(pointInt.y()));
        }, () -> state.getUtilities().logIllegalState("cannot parse coordinates from: {}", value));
    }

    public String getDataValue() {
        return CommonUtil.pointToString(new PointInt(Integer.parseInt(xField.getText()), Integer.parseInt(yField.getText())));
    }
}
