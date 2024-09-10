package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.*;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

class CommandEditPane extends JPanel {

    private final ArrayList<CommandDataTypePane> commandDataList;
    private final JPanel commandDataPane;
    private final JComboBox<CommandType> commandTypeComboBox;

    public CommandEditPane() {
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        commandDataList = new ArrayList<>();
        commandTypeComboBox = createCommandTypeComboBox();
        commandTypeComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        commandTypeComboBox.setSelectedIndex(-1);
        add(commandTypeComboBox);
        commandDataPane = new JPanel();
        commandDataPane.setAlignmentX(Component.LEFT_ALIGNMENT);
        commandDataPane.setLayout(new BoxLayout(commandDataPane, BoxLayout.PAGE_AXIS));
        add(commandDataPane);
    }

    public CommandEditPane(final Command command) {
        this();
        commandTypeComboBox.setSelectedItem(command.getType());
        command.getData().forEach((commandDataType, s) -> commandDataList.add(new CommandDataTypePane(commandDataType, s, getAvailableDataTypes(command.getType()))));
        commandDataList.forEach(commandDataPane::add);
    }

    private JComboBox<CommandType> createCommandTypeComboBox() {
        final JComboBox<CommandType> commandType = new JComboBox<>(CommandType.values());
        Util.retainComponentSize(commandType);
        commandType.addActionListener((e) -> {
            final CommandType selectedItem = (CommandType) commandType.getSelectedItem();
            if (selectedItem != null) initCommandData(selectedItem);
        });
        return commandType;
    }

    private void initCommandData(final CommandType commandType) {
        commandDataList.clear();
        final List<CommandDataType> availableDataTypes = getAvailableDataTypes(commandType);
        commandDataList.add(new CommandDataTypePane(availableDataTypes));
        repaintCommandData();
    }

    private static List<CommandDataType> getAvailableDataTypes(final CommandType commandType) {
        final List<CommandDataType> availableDataTypes = new ArrayList<>();
        availableDataTypes.addAll(commandType.mandatory().stream().flatMap(Collection::stream).toList());
        availableDataTypes.addAll(commandType.optional());
        return availableDataTypes;
    }

    private void addDataAbove(final CommandDataTypePane commandDataTypePane) {
        final int currentIndex = commandDataList.indexOf(commandDataTypePane);
        commandDataList.add(currentIndex, new CommandDataTypePane(commandDataTypePane.getAvailableDataTypes()));
        repaintCommandData();
    }

    private void deleteData(final CommandDataTypePane checkDataTypePane) {
        if (commandDataList.size() == 1) {
            JOptionPane.showMessageDialog(this, "You cannot delete last check data");
            return;
        }
        commandDataList.remove(checkDataTypePane);
        repaintCommandData();

    }

    private void moveDataUp(final CommandDataTypePane checkDataTypePane) {
        final int currentIndex = commandDataList.indexOf(checkDataTypePane);
        if (currentIndex == 0) return;
        commandDataList.remove(checkDataTypePane);
        commandDataList.add(currentIndex - 1, checkDataTypePane);
        repaintCommandData();
    }

    private void moveDataDown(final CommandDataTypePane checkDataTypePane) {
        final int currentIndex = commandDataList.indexOf(checkDataTypePane);
        if (currentIndex == commandDataList.size() - 1) return;
        commandDataList.remove(checkDataTypePane);
        commandDataList.add(currentIndex + 1, checkDataTypePane);
        repaintCommandData();
    }

    private void repaintCommandData() {
        commandDataPane.removeAll();
        commandDataList.forEach(commandDataPane::add);
        revalidate();
    }

    private class CommandDataTypePane extends JPanel {

        final List<CommandDataType> availableDataTypes;

        public CommandDataTypePane(final CommandDataType commandDataType, final String dataValue, final List<CommandDataType> availableDataTypes) {
            this.availableDataTypes = availableDataTypes;
            setAlignmentX(Component.LEFT_ALIGNMENT);
            setLayout(new BorderLayout());
            final JPanel buttonPane = createButtonPane();
            add(buttonPane, BorderLayout.PAGE_START);
            final JPanel dataPane = createDataPane(commandDataType, dataValue, availableDataTypes);
            add(dataPane, BorderLayout.CENTER);
        }

        public CommandDataTypePane(final List<CommandDataType> availableDataTypes) {
            this(null, null, availableDataTypes);
        }

        public List<CommandDataType> getAvailableDataTypes() {
            return availableDataTypes;
        }

        private JPanel createDataPane(final CommandDataType commandDataType, final String dataValue, final List<CommandDataType> availableDataTypes) {
            final JPanel dataPane = new JPanel();
            dataPane.setLayout(new BoxLayout(dataPane, BoxLayout.LINE_AXIS));
            final JComboBox<CommandDataType> dataTypeComboBox = new JComboBox<>(availableDataTypes.toArray(new CommandDataType[]{}));
            Util.retainComponentSize(dataTypeComboBox);
            if (commandDataType != null) {
                dataTypeComboBox.setSelectedItem(commandDataType);
            } else {
                dataTypeComboBox.setSelectedIndex(-1);
            }
            dataPane.add(dataTypeComboBox);
            final JTextField valueField = new JTextField(dataValue);
            Util.retainComponentSize(valueField);
            dataPane.add(valueField);
            return dataPane;
        }

        private JPanel createButtonPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
            panel.setSize(panel.getPreferredSize());
//            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddAboveButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddAboveButton() {
            final Button btn = new Button("Add above");
            btn.addActionListener(e -> addDataAbove(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("Delete");
            btn.addActionListener(e -> deleteData(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("Move up");
            btn.addActionListener(e -> moveDataUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("Move down");
            btn.addActionListener(e -> moveDataDown(this));
            return btn;
        }

    }
}
