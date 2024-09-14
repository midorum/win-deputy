package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.*;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

class CommandEditPane extends JPanel {

    private final State state;
    private final ArrayList<CommandDataTypePane> commandDataList;
    private final JPanel commandDataPane;
    private final JComboBox<CommandType> commandTypeComboBox;

    public CommandEditPane(final State state) {
        this.state = state;
        commandDataList = new ArrayList<>();
        commandTypeComboBox = createCommandTypeComboBox();
        commandTypeComboBox.setSelectedIndex(-1);
        commandDataPane = new JPanel();
        Util.putComponentsToVerticalGrid(this, commandTypeComboBox, commandDataPane);
        this.setBorder(BorderFactory.createLineBorder(Color.CYAN));
    }

    public CommandEditPane(final Command command, final State state) {
        this(state);
        commandTypeComboBox.setSelectedItem(command.getType());
        command.getData().forEach((commandDataType, s) -> commandDataList.add(new CommandDataTypePane(commandDataType, s, getAvailableDataTypes(command.getType()))));
        commandDataList.forEach(commandDataPane::add);
    }

    private JComboBox<CommandType> createCommandTypeComboBox() {
        final JComboBox<CommandType> commandType = new JComboBox<>(CommandType.values());
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

    private void addDataBelow(final CommandDataTypePane commandDataTypePane) {
        final int currentIndex = commandDataList.indexOf(commandDataTypePane);
        commandDataList.add(currentIndex + 1, new CommandDataTypePane(commandDataTypePane.getAvailableDataTypes()));
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
        Util.putComponentsToVerticalGrid(commandDataPane, -1, commandDataList.toArray(Component[]::new));
        revalidate();
    }

    private class CommandDataTypePane extends JPanel {

        final List<CommandDataType> availableDataTypes;
        final private JPanel sourceTypeEditPane;

        public CommandDataTypePane(final CommandDataType commandDataType, final String dataValue, final List<CommandDataType> availableDataTypes) {
            this.availableDataTypes = availableDataTypes;
            this.sourceTypeEditPane = new JPanel();
            Util.putComponentsToHorizontalGrid(this, createButtonPane(), createDataPane(commandDataType, dataValue, availableDataTypes));
        }

        public CommandDataTypePane(final List<CommandDataType> availableDataTypes) {
            this(null, null, availableDataTypes);
        }

        public List<CommandDataType> getAvailableDataTypes() {
            return availableDataTypes;
        }

        private JPanel createDataPane(final CommandDataType commandDataType, final String dataValue, final List<CommandDataType> availableDataTypes) {
            final JPanel dataPane = new JPanel();
            Util.putComponentsToHorizontalGrid(dataPane,
                    createDataTypeComboBox(commandDataType, availableDataTypes),
                    sourceTypeEditPane);
            if (commandDataType != null) {
                fillSourceTypeEditGrid(commandDataType, dataValue);
            }
            return dataPane;
        }

        private JComboBox<CommandDataType> createDataTypeComboBox(final CommandDataType commandDataType, final List<CommandDataType> availableDataTypes) {
            final JComboBox<CommandDataType> dataTypeComboBox = new JComboBox<>(availableDataTypes.toArray(new CommandDataType[]{}));
            if (commandDataType != null) {
                dataTypeComboBox.setSelectedItem(commandDataType);
            } else {
                dataTypeComboBox.setSelectedIndex(-1);
            }
            dataTypeComboBox.addActionListener((e) -> {
                final CommandDataType selectedItem = (CommandDataType) dataTypeComboBox.getSelectedItem();
                if (selectedItem != null) repaintSourceTypeEditPane(selectedItem);
            });
            return dataTypeComboBox;
        }

        private void repaintSourceTypeEditPane(final CommandDataType commandDataType) {
            sourceTypeEditPane.removeAll();
            fillSourceTypeEditGrid(commandDataType, null);
            revalidate();
        }

        private void fillSourceTypeEditGrid(final CommandDataType commandDataType, final String dataValue) {
            Util.putComponentsToHorizontalGrid(sourceTypeEditPane, new SourceTypeEditPane(commandDataType.getSourceTypes(), dataValue, state));
        }

        private JPanel createButtonPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
            panel.setSize(panel.getPreferredSize());
            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(e -> addDataBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(e -> deleteData(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(e -> moveDataUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(e -> moveDataDown(this));
            return btn;
        }

    }
}
