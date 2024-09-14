package midorum.win32.deputy.ui;

import com.midorum.win32api.facade.Win32System;
import com.midorum.win32api.struct.PointInt;
import com.midorum.win32api.win32.MsLcid;
import midorum.win32.deputy.model.SourceType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

class SourceTypeEditPane extends JPanel {

    private final State state;
    private final Component valueField;
    private final Logger logger = LogManager.getLogger(this);

    public SourceTypeEditPane(final List<SourceType> sourceTypes, final String dataValue, final State state) {
        this.state = state;
        final List<Component> components = new ArrayList<>();
        this.valueField = sourceTypes.contains(SourceType.userInput) ? new JTextField(dataValue)
                : sourceTypes.contains(SourceType.keyboardLayoutChoice) ? createKeyboardLayoutComboBox(dataValue)
                : sourceTypes.contains(SourceType.booleanChoice) ? createBooleanChoiceComboBox(dataValue)
                : sourceTypes.contains(SourceType.coordinatesInput) ? new CoordinatesInput(dataValue)
                : new JLabel(dataValue);
        components.add(valueField);
        if (sourceTypes.contains(SourceType.pickFile)) {
            final Button btn = new Button("...");
            btn.addActionListener(event -> Util.pickFile(state, this, file -> setValueField(file.getName())));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.makeShot)) {
            final Button btn = new Button("[o]");
            btn.addActionListener(event -> Util.captureAndSaveRegion(this.state, this,
                    savedFile -> setValueField(savedFile.getName()),
                    e -> {
                        logger.error(e.getMessage(), e);
                        JOptionPane.showMessageDialog(this, e.getMessage());
                    }));
            components.add(btn);
        }
        Util.putComponentsToHorizontalGridStretchFirst(this, components.toArray(Component[]::new));
    }

    private Component createKeyboardLayoutComboBox(final String dataValue) {
        final List<String> layouts = new ArrayList<>();
        Win32System.getInstance().getAvailableKeyboardLayouts()
                .consumeOrHandleError((Consumer<? super MsLcid[]>) msLcids ->
                                Arrays.stream(msLcids).forEach(msLcid -> layouts.add(msLcid.localeName())),
                        e -> {
                            logger.error("Cannot obtain available keyboard layouts", e);
                            layouts.add("en-US");
                        });
        final JComboBox<String> comboBox = new JComboBox<>(layouts.toArray(String[]::new));
        if (dataValue != null) {
            comboBox.setSelectedItem(dataValue);
        } else {
            comboBox.setSelectedIndex(-1);
        }
        return comboBox;
    }

    private Component createBooleanChoiceComboBox(final String dataValue) {
        final JComboBox<String> comboBox = new JComboBox<>(new String[]{Boolean.TRUE.toString(), Boolean.FALSE.toString()});
        if (dataValue != null) {
            comboBox.setSelectedItem(dataValue);
        } else {
            comboBox.setSelectedIndex(-1);
        }
        return comboBox;
    }

    private void setValueField(final String value) {
        switch (valueField) {
            case JTextField f -> f.setText(value);
            case JLabel f -> f.setText(value);
            case JComboBox<?> f -> {
                if (value != null) {
                    f.setSelectedItem(value);
                } else {
                    f.setSelectedIndex(-1);
                }
            }
            case CoordinatesInput f -> f.setDataValue(value);
            default -> throw new IllegalArgumentException("Unrecognized component");
        }
    }

    private String getDataValue() {
        return switch (valueField) {
            case JTextField f -> f.getText();
            case JLabel f -> f.getText();
            case JComboBox<?> f -> f.getSelectedItem() != null ? f.getSelectedItem().toString() : null;
            case CoordinatesInput f -> f.getDataValue();
            default -> throw new IllegalArgumentException("Unrecognized component");
        };
    }

    private class CoordinatesInput extends JPanel {

        private final IntegerTextField xField;
        private final IntegerTextField yField;

        public CoordinatesInput(final String value) {
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

}
