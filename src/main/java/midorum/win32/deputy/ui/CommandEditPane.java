package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import midorum.win32.deputy.model.*;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.function.Supplier;

class CommandEditPane extends JPanel implements SupplierThrowing<Command, IllegalInputException> {

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
        SwingUtil.putComponentsToVerticalGrid(this, commandTypeComboBox, commandDataPane);
    }

    public CommandEditPane(final Command command, final State state) {
        this(state);
        commandTypeComboBox.setSelectedItem(command.getType());
        final List<CommandDataType> availableDataTypes = getAvailableDataTypes(command.getType());
        commandDataList.clear();
        command.getData().forEach((commandDataType, s) -> commandDataList.add(new CommandDataTypePane(commandDataType, s, availableDataTypes)));
        repaintCommandData();
    }

    private JComboBox<CommandType> createCommandTypeComboBox() {
        final JComboBox<CommandType> commandType = new JComboBox<>(CommandType.values());
        commandType.addActionListener((e) -> {
            final CommandType selectedItem = (CommandType) commandType.getSelectedItem();
            if (selectedItem != null) {
                commandDataList.clear();
                commandDataList.add(new CommandDataTypePane(getAvailableDataTypes(selectedItem)));
                repaintCommandData();
            }
        });
        return commandType;
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
            JOptionPane.showMessageDialog(this, "You cannot delete last command data");
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
        SwingUtil.putComponentsToVerticalGrid(commandDataPane, -1, commandDataList.toArray(Component[]::new));
        revalidate();
    }

    @Override
    public Command get() throws IllegalInputException {
        final CommandType commandType = validateAndGetCommandType();
        return new Command(commandType, validateAndGetCommandData(commandType));
    }

    private CommandType validateAndGetCommandType() throws IllegalInputException {
        final CommandType commandType = (CommandType) commandTypeComboBox.getSelectedItem();
        if (!Command.validateCommandType(commandType)) {
            commandTypeComboBox.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal command type");
        }
        commandTypeComboBox.setBorder(null);
        return commandType;
    }

    private Map<CommandDataType, String> validateAndGetCommandData(final CommandType commandType) throws IllegalInputException {
        final Map<CommandDataType, String> data = new HashMap<>(commandDataList.size());
        for (final CommandDataTypePane next : commandDataList) {
            final CommandDataType commandDataType = next.getCommandDataType();
            final String commandDataValue = next.getCommandDataValue();
            if (!Command.validateCommandDataEntry(commandDataType, commandDataValue)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException("Illegal command data");
            }
            next.setBorder(null);
            data.put(commandDataType, commandDataValue);
        }
        if (!Command.validateCommandData(commandType, data)) {
            commandDataPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal check data");
        }
        commandDataPane.setBorder(null);
        return data;
    }

    private class CommandDataTypePane extends JPanel {

        final List<CommandDataType> availableDataTypes;
        final private JPanel sourceTypeEditPaneWrapper;
        private JComboBox<CommandDataType> dataTypeComboBox;
        private Supplier<String> dataValueSupplier;

        public CommandDataTypePane(final List<CommandDataType> availableDataTypes) {
            this.availableDataTypes = availableDataTypes;
            this.sourceTypeEditPaneWrapper = new JPanel();
            this.dataTypeComboBox = createDataTypeComboBox(availableDataTypes);
            SwingUtil.putComponentsToHorizontalGrid(this,
                    createButtonPane(),
                    dataTypeComboBox,
                    sourceTypeEditPaneWrapper);
        }

        public CommandDataTypePane(final CommandDataType commandDataType, final String dataValue, final List<CommandDataType> availableDataTypes) {
            this(availableDataTypes);
            dataTypeComboBox.setSelectedItem(commandDataType);
            fillSourceTypeEditGrid(commandDataType, dataValue);
        }

        public List<CommandDataType> getAvailableDataTypes() {
            return availableDataTypes;
        }

        private JComboBox<CommandDataType> createDataTypeComboBox(final List<CommandDataType> availableDataTypes) {
            final JComboBox<CommandDataType> comboBox = new JComboBox<>(availableDataTypes.toArray(new CommandDataType[]{}));
            comboBox.setSelectedIndex(-1);
            comboBox.addActionListener((e) -> {
                final CommandDataType selectedItem = (CommandDataType) comboBox.getSelectedItem();
                if (selectedItem != null) fillSourceTypeEditGrid(selectedItem, null);
            });
            return comboBox;
        }

        private void fillSourceTypeEditGrid(final CommandDataType commandDataType, final String dataValue) {
            final SourceTypeEditPane sourceTypeEditPane = new SourceTypeEditPane(commandDataType.getSourceTypes(), dataValue, state);
            dataValueSupplier = sourceTypeEditPane;
            sourceTypeEditPaneWrapper.removeAll();
            SwingUtil.putComponentsToHorizontalGrid(sourceTypeEditPaneWrapper, sourceTypeEditPane);
            sourceTypeEditPaneWrapper.revalidate();
        }

        private JPanel createButtonPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
            panel.setSize(panel.getPreferredSize());
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

        public CommandDataType getCommandDataType() {
            return (CommandDataType) dataTypeComboBox.getSelectedItem();
        }

        public String getCommandDataValue() {
            return dataValueSupplier != null ? dataValueSupplier.get() : null;
        }
    }
}
