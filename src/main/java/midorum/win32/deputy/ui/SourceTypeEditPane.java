package midorum.win32.deputy.ui;

import com.midorum.win32api.facade.Win32System;
import com.midorum.win32api.win32.MsLcid;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.model.SourceType;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

class SourceTypeEditPane extends JPanel implements Supplier<String> {

    private final Component valueField;
    private final State state;

    public SourceTypeEditPane(final List<SourceType> sourceTypes, final String dataValue, final State state) {
        this.state = state;
        final List<Component> components = new ArrayList<>();
        this.valueField = sourceTypes.contains(SourceType.userInput) ? new JTextField(dataValue)
                : sourceTypes.contains(SourceType.keyboardLayoutChoice) ? createKeyboardLayoutComboBox(dataValue)
                : sourceTypes.contains(SourceType.booleanChoice) ? createBooleanChoiceComboBox(dataValue)
                : sourceTypes.contains(SourceType.coordinatesInput) ? new CoordinatesInput(dataValue, state)
                : new JLabel(dataValue);
        components.add(valueField);
        if (sourceTypes.contains(SourceType.pickFile)) {
            final Button btn = new Button("...");
            btn.addActionListener(event -> state.getUtilities().pickFile(state,
                    file -> setValueField(file.getName())));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.makeShot)) {
            final Button btn = new Button("[o]");
            btn.addActionListener(event -> state.getUtilities().captureAndSaveRegion(state,
                    savedFile -> setValueField(savedFile.getName())));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.pickAbsolutePoint)) {
            final Button btn = new Button("+");
            btn.addActionListener(event -> state.getUtilities().pickAbsolutePoint(pointInt ->
                    setValueField(CommonUtil.pointToString(pointInt))));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.pickWindowProcessName)) {
            final Button btn = new Button("+");
            btn.addActionListener(event -> state.getUtilities().pickWindowProcessName(this::setValueField));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.pickWindowClassName)) {
            final Button btn = new Button("+");
            btn.addActionListener(event -> state.getUtilities().pickWindowClassName(this::setValueField));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.pickWindowTitle)) {
            final Button btn = new Button("+");
            btn.addActionListener(event -> state.getUtilities().pickWindowTitle(this::setValueField));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.pickWindowStyles)) {
            final Button btn = new Button("+");
            btn.addActionListener(event -> state.getUtilities().pickWindowStyles(this::setValueField));
            components.add(btn);
        }
        if (sourceTypes.contains(SourceType.pickWindowExStyles)) {
            final Button btn = new Button("+");
            btn.addActionListener(event -> state.getUtilities().pickWindowExStyles(this::setValueField));
            components.add(btn);
        }
        SwingUtil.putComponentsToHorizontalGrid(this, 0, components.toArray(Component[]::new));
    }

    private Component createKeyboardLayoutComboBox(final String dataValue) {
        final List<String> layouts = new ArrayList<>();
        Win32System.getInstance().getAvailableKeyboardLayouts()
                .consumeOrHandleError((Consumer<? super MsLcid[]>) msLcids ->
                                Arrays.stream(msLcids).forEach(msLcid -> layouts.add(msLcid.localeName())),
                        e -> {
                            state.getUtilities().logThrowable("Cannot obtain available keyboard layouts", e);
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

    @Override
    public String get() {
        return CommonUtil.trimToNull(switch (valueField) {
            case JTextField f -> f.getText();
            case JLabel f -> f.getText();
            case JComboBox<?> f -> f.getSelectedItem() != null ? f.getSelectedItem().toString() : null;
            case CoordinatesInput f -> f.getDataValue();
            default -> throw new IllegalArgumentException("Unrecognized component");
        });
    }
}
