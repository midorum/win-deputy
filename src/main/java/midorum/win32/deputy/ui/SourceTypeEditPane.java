package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.SourceType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

class SourceTypeEditPane extends JPanel {

    private final State state;
    private final Component valueField;
    private final Logger logger = LogManager.getLogger(this);

    public SourceTypeEditPane(final List<SourceType> sourceTypes, final String dataValue, final State state) {
        this.state = state;
        final List<Component> components = new ArrayList<>();
        this.valueField = sourceTypes.contains(SourceType.userInput) ? new JTextField(dataValue) : new JLabel(dataValue);
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

    private void setValueField(final String value) {
        switch (valueField) {
            case JTextField f -> f.setText(value);
            case JLabel f -> f.setText(value);
            default -> throw new IllegalArgumentException("Unrecognized component");
        }
    }

    private String getDataValue() {
        return switch (valueField) {
            case JTextField f -> f.getText();
            case JLabel f -> f.getText();
            default -> throw new IllegalArgumentException("Unrecognized component");
        };
    }

}
