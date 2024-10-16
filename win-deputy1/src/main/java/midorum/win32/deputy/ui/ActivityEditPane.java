package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
import dma.validation.Validator;
import midorum.win32.deputy.i18n.UiElement;
import midorum.win32.deputy.model.*;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class ActivityEditPane extends JPanel implements SupplierThrowing<Activity, IllegalInputException> {

    private final State state;
    private final JTextField titleField;
    private final JTextArea descriptionField;
    private final List<CheckWrapperPane> checks;
    private final List<CommandWrapperPane> commands;
    private final JPanel checksPane;
    private final JPanel commandsPane;
    private final WaitingListEditWrapperPane waitingListEditWrapperPane;
    private final JCheckBox repeatableCheckBox;
    private final JCheckBox producesFragileStateCheckBox;

    ActivityEditPane(final Activity activity, final State state) {
        final Activity activityChecked = Validator.checkNotNull(activity).orThrowForSymbol("activity");
        this.state = Validator.checkNotNull(state).orThrowForSymbol("state");
        this.checks = new ArrayList<>();
        final List<Check> activityChecks = activityChecked.getChecks();
        if (activityChecks != null) {
            checks.addAll(activityChecks.stream()
                    .map(check -> new CheckWrapperPane(check, state))
                    .collect(Collectors.toCollection(ArrayList::new)));
        } else {
            checks.add(new CheckWrapperPane(state));
        }
        this.commands = new ArrayList<>();
        final List<Command> activityCommands = activityChecked.getCommands();
        if (activityCommands != null) {
            commands.addAll(activityCommands.stream()
                    .map(command -> new CommandEditPane(command, state))
                    .map(CommandWrapperPane::new)
                    .collect(Collectors.toCollection(ArrayList::new)));
        } else {
            commands.add(new CommandWrapperPane(state));
        }
        this.titleField = new JTextField(activityChecked.getTitle());
        this.descriptionField = new JTextArea(activityChecked.getDescription().orElse(null));
        this.checksPane = new JPanel(new GridBagLayout());
        checksPane.setBorder(BorderFactory.createLineBorder(Color.GREEN, 2));
        this.commandsPane = new JPanel(new GridBagLayout());
        commandsPane.setBorder(BorderFactory.createLineBorder(Color.BLUE, 2));
        this.waitingListEditWrapperPane = activityChecked.getWaitingList()
                .map(waitingList -> new WaitingListEditWrapperPane(waitingList, state))
                .orElse(new WaitingListEditWrapperPane(state));
        waitingListEditWrapperPane.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY, 2));
        this.repeatableCheckBox = new JCheckBox(UiElement.repeatableLabel.forUserLocale(), activityChecked.isRepeatable());
        this.producesFragileStateCheckBox = new JCheckBox(UiElement.producesFragileStateLabel.forUserLocale(),
                activityChecked.producesFragileState());
        SwingUtil.putComponentsToVerticalGrid(this,
                -1,
                titleField,
                descriptionField,
                repeatableCheckBox,
                producesFragileStateCheckBox,
                checksPane,
                commandsPane,
                waitingListEditWrapperPane);
        fillChecksGrid();
        fillCommandsGrid();
    }

    private void addCheckAbove(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        checks.add(currentIndex, new CheckWrapperPane(state));
        repaintChecks();
    }

    private void addCheckBelow(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        checks.add(currentIndex + 1, new CheckWrapperPane(state));
        repaintChecks();
    }

    private void deleteCheck(final CheckWrapperPane checkPane) {
        if (checks.size() == 1) {
            JOptionPane.showMessageDialog(this, UiElement.cannotDeleteLastCheck.forUserLocale());
            return;
        }
        checks.remove(checkPane);
        repaintChecks();
    }

    private void moveCheckUp(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        if (currentIndex == 0) return;
        checks.remove(checkPane);
        checks.add(currentIndex - 1, checkPane);
        repaintChecks();
    }

    private void moveCheckDown(final CheckWrapperPane checkPane) {
        final int currentIndex = checks.indexOf(checkPane);
        if (currentIndex == checks.size() - 1) return;
        checks.remove(checkPane);
        checks.add(currentIndex + 1, checkPane);
        repaintChecks();
    }

    private boolean canIgnoreCheck() {
        if (checks.stream().filter(checkWrapperPane -> !checkWrapperPane.isIgnore()).findAny().isEmpty()) {
            JOptionPane.showMessageDialog(this, UiElement.cannotIgnoreLastCheck.forUserLocale());
            return false;
        }
        return true;
    }

    private void repaintChecks() {
        checksPane.removeAll();
        fillChecksGrid();
        revalidate();
    }

    private void addCommandAbove(final CommandWrapperPane commandPane) {
        final int currentIndex = commands.indexOf(commandPane);
        commands.add(currentIndex, new CommandWrapperPane(state));
        repaintCommands();
    }

    private void addCommandBelow(final CommandWrapperPane commandPane) {
        final int currentIndex = commands.indexOf(commandPane);
        commands.add(currentIndex + 1, new CommandWrapperPane(state));
        repaintCommands();
    }

    private void deleteCommand(final CommandWrapperPane commandPane) {
        if (commands.size() == 1) {
            JOptionPane.showMessageDialog(this, UiElement.cannotDeleteLastCommand.forUserLocale());
            return;
        }
        commands.remove(commandPane);
        repaintCommands();
    }

    private void moveCommandUp(final CommandWrapperPane commandPane) {
        final int currentIndex = commands.indexOf(commandPane);
        if (currentIndex == 0) return;
        commands.remove(commandPane);
        commands.add(currentIndex - 1, commandPane);
        repaintCommands();
    }

    private void moveCommandDown(final CommandWrapperPane commandPane) {
        final int currentIndex = commands.indexOf(commandPane);
        if (currentIndex == commands.size() - 1) return;
        commands.remove(commandPane);
        commands.add(currentIndex + 1, commandPane);
        repaintCommands();
    }

    private void repaintCommands() {
        commandsPane.removeAll();
        fillCommandsGrid();
        revalidate();
    }

    private void fillChecksGrid() {
        SwingUtil.putComponentsToVerticalGrid(checksPane, checks.toArray(Component[]::new));
    }

    private void fillCommandsGrid() {
        SwingUtil.putComponentsToVerticalGrid(commandsPane, commands.toArray(Component[]::new));
    }

    @Override
    public Activity get() throws IllegalInputException {
        return new Activity(validateAndGetActivityTitle(),
                descriptionField.getText(),
                validateAndGetChecks(),
                validateAndGetCommands(),
                waitingListEditWrapperPane.get(),
                repeatableCheckBox.isSelected(),
                producesFragileStateCheckBox.isSelected(),
                false);
    }

    private String validateAndGetActivityTitle() throws IllegalInputException {
        final String title = titleField.getText();
        if (title == null || title.isBlank()) {
            titleField.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException(UiElement.activityTitleCannotBeEmpty);
        }
        titleField.setBorder(null);
        return title;
    }

    private List<Check> validateAndGetChecks() throws IllegalInputException {
        final List<Check> checkList = new ArrayList<>();
        for (CheckWrapperPane next : checks) {
            final Check check = next.get();
            if (!Activity.validateCheckListItem(check)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException(UiElement.illegalCheck);
            }
            next.setBorder(null);
            checkList.add(check);
        }
        if (!Activity.validateChecks(checkList)) {
            checksPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException(UiElement.illegalChecks);
        }
        checksPane.setBorder(null);
        return checkList;
    }

    private List<Command> validateAndGetCommands() throws IllegalInputException {
        final List<Command> commandList = new ArrayList<>();
        for (CommandWrapperPane next : commands) {
            final Command command = next.get();
            if (!Activity.validateCommandListItem(command)) {
                next.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
                throw new IllegalInputException(UiElement.illegalCommand);
            }
            next.setBorder(null);
            commandList.add(command);
        }
        if (!Activity.validateCommands(commandList)) {
            commandsPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException(UiElement.illegalCommands);
        }
        commandsPane.setBorder(null);
        return commandList;
    }

    private class CheckWrapperPane extends JPanel implements SupplierThrowing<Check, IllegalInputException> {

        private final CheckEditPane checkEditPane;
        private final JCheckBox ignoreCheckBox;

        public CheckWrapperPane(final Check check, final State state) {
            this.checkEditPane = new CheckEditPane(check, state);
            this.ignoreCheckBox = createIgnoreCheckBox(check.isIgnore());
            SwingUtil.putComponentsToVerticalGrid(this, -1, createButtonsPane(ignoreCheckBox), checkEditPane);
        }

        public CheckWrapperPane(final State state) {
            this.checkEditPane = new CheckEditPane(state);
            this.ignoreCheckBox = createIgnoreCheckBox(false);
            SwingUtil.putComponentsToVerticalGrid(this, -1, createButtonsPane(ignoreCheckBox), checkEditPane);
        }

        private boolean isIgnore() {
            return ignoreCheckBox.isSelected();
        }

        private JPanel createButtonsPane(final Component... components) {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel(UiElement.checkLabel.forUserLocale()));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            if (components != null) for (final Component c : components) panel.add(c);
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(_ -> addCheckBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(_ -> deleteCheck(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(_ -> moveCheckUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(_ -> moveCheckDown(this));
            return btn;
        }

        private JCheckBox createIgnoreCheckBox(final boolean ignore) {
            final JCheckBox checkBox;
            checkBox = new JCheckBox(UiElement.ignoreLabel.forUserLocale());
            checkBox.addActionListener(_ -> {
                if (checkBox.isSelected() && !canIgnoreCheck()) {
                    checkBox.setSelected(false);
                }
            });
            checkBox.setSelected(ignore);
            return checkBox;
        }

        @Override
        public Check get() throws IllegalInputException {
            final Check check = checkEditPane.get();
            return new Check(check.getType(), check.getData(), ignoreCheckBox.isSelected());
        }
    }

    private class CommandWrapperPane extends JPanel implements SupplierThrowing<Command, IllegalInputException> {

        private final CommandEditPane commandEditPane;

        private CommandWrapperPane(final CommandEditPane commandEditPane) {
            this.commandEditPane = commandEditPane;
            SwingUtil.putComponentsToVerticalGrid(this, -1, createButtonsPane(), commandEditPane);
        }

        public CommandWrapperPane(final State state) {
            this(new CommandEditPane(state));
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel(UiElement.commandLabel.forUserLocale()));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(_ -> addCommandBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(_ -> deleteCommand(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(_ -> moveCommandUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(_ -> moveCommandDown(this));
            return btn;
        }

        @Override
        public Command get() throws IllegalInputException {
            return commandEditPane.get();
        }
    }

    private static class WaitingListEditWrapperPane extends JPanel implements SupplierThrowing<WaitingList, IllegalInputException> {

        private final State state;
        private final JPanel container;
        private WaitingListEditPane waitingListEditPane;
        private Button addWaitingButton;
        private Button deleteWaitingButton;

        public WaitingListEditWrapperPane(final State state) {
            this.state = state;
            this.container = new JPanel();
            SwingUtil.putComponentsToVerticalGrid(this,
                    createButtonsPane(),
                    container);
        }

        public WaitingListEditWrapperPane(final WaitingList waitingList, final State state) {
            this(state);
            addWaitingList(waitingList, state);
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel();
            addWaitingButton = new Button("+");
            addWaitingButton.addActionListener(_ -> addWaitingList(new WaitingList(), state));
            deleteWaitingButton = new Button("-");
            deleteWaitingButton.addActionListener(_ -> deleteWaitingList());
            SwingUtil.putComponentsToHorizontalGrid(panel,
                    0,
                    new JLabel(UiElement.waitingListLabel.forUserLocale()),
                    addWaitingButton,
                    deleteWaitingButton);
            return panel;
        }

        private void addWaitingList(final WaitingList waitingList, final State state) {
            waitingListEditPane = new WaitingListEditPane(waitingList, state);
            SwingUtil.putComponentsToVerticalGrid(container, waitingListEditPane);
            addWaitingButton.setVisible(false);
            deleteWaitingButton.setVisible(true);
            revalidate();
        }

        private void deleteWaitingList() {
            waitingListEditPane = null;
            container.removeAll();
            addWaitingButton.setVisible(true);
            deleteWaitingButton.setVisible(false);
            revalidate();
        }

        @Override
        public WaitingList get() throws IllegalInputException {
            return waitingListEditPane != null ? waitingListEditPane.get() : null;
        }
    }
}
