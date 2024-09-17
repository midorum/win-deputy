package midorum.win32.deputy.ui;

import com.midorum.win32api.struct.PointInt;
import midorum.win32.deputy.common.CommonUtil;

import javax.swing.*;
import java.awt.*;

class CoordinatesInput extends JPanel {

    private final State state;
    private final IntegerTextField xField;
    private final IntegerTextField yField;

    CoordinatesInput(final State state) {
        this.state = state;
        final DisplayMode currentDisplayMode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        final int topWidth = currentDisplayMode.getWidth() - 1;
        final int topHeight = currentDisplayMode.getHeight() - 1;
        xField = new IntegerTextField(0, topWidth);
        yField = new IntegerTextField(0, topHeight);
        SwingUtil.putComponentsToHorizontalGrid(this,
                new double[]{0.0, 0.5, 0.0, 0.5},
                new JLabel("x (0-" + topWidth + ")"),
                xField,
                new JLabel("y (0-" + topHeight + ")"),
                yField
        );
    }

    CoordinatesInput(final String value, final State state) {
        this(state);
        if (value != null) {
            CommonUtil.stringToPoint(value).ifPresentOrElse(pointInt -> {
                xField.setText(Integer.toString(pointInt.x()));
                yField.setText(Integer.toString(pointInt.y()));
            }, () -> state.getUtilities().reportIllegalState("Cannot parse coordinates: " + value));
        }
    }

    public void setDataValue(final String value) {
        CommonUtil.stringToPoint(value).ifPresentOrElse(pointInt -> {
            xField.setText(Integer.toString(pointInt.x()));
            yField.setText(Integer.toString(pointInt.y()));
        }, () -> state.getUtilities().logIllegalState("Cannot parse coordinates from: " + value));
    }

    public String getDataValue() {
        return CommonUtil.pointToString(new PointInt(Integer.parseInt(xField.getText()), Integer.parseInt(yField.getText())));
    }
}
