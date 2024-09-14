package midorum.win32.deputy.ui;

import com.midorum.win32api.struct.PointInt;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;

class CoordinatesInput extends JPanel {

    private final IntegerTextField xField;
    private final IntegerTextField yField;
    private final Logger logger = LogManager.getLogger(this);

    CoordinatesInput(final String value) {
        final DisplayMode currentDisplayMode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        final int topWidth = currentDisplayMode.getWidth() - 1;
        final int topHeight = currentDisplayMode.getHeight() - 1;
        xField = new IntegerTextField(0, topWidth);
        yField = new IntegerTextField(0, topHeight);
        Util.putComponentsToHorizontalGrid(this,
                new double[]{0.0, 0.5, 0.0, 0.5},
                new JLabel("x (0-" + topWidth + ")"),
                xField,
                new JLabel("y (0-" + topHeight + ")"),
                yField
        );
    }

    public void setDataValue(final String value) {
        Util.stringToPoint(value).ifPresentOrElse(pointInt -> {
            xField.setText(Integer.toString(pointInt.x()));
            yField.setText(Integer.toString(pointInt.y()));
        }, () -> logger.error("Cannot parse coordinates from: {}", value));
    }

    public String getDataValue() {
        return Util.pointToString(new PointInt(Integer.parseInt(xField.getText()), Integer.parseInt(yField.getText())));
    }
}
