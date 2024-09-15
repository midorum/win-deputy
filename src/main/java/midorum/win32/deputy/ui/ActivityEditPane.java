package midorum.win32.deputy.ui;

import dma.function.SupplierThrowing;
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
    private final WaitingEditWrapperPane waitingEditWrapperPane;

    ActivityEditPane(final Activity activity, final State state) {
        this.state = state;
        checks = new ArrayList<>();
        final List<Check> activityChecks = activity.getChecks();
        if (activityChecks != null) {
            checks.addAll(activityChecks.stream()
                    .map(check -> new CheckEditPane(check, state))
                    .map(CheckWrapperPane::new)
                    .collect(Collectors.toCollection(ArrayList::new)));
        } else {
            checks.add(new CheckWrapperPane(state));
        }
        commands = new ArrayList<>();
        final List<Command> activityCommands = activity.getCommands();
        if (activityCommands != null) {
            commands.addAll(activityCommands.stream()
                    .map(command -> new CommandEditPane(command, state))
                    .map(CommandWrapperPane::new)
                    .collect(Collectors.toCollection(ArrayList::new)));
        } else {
            commands.add(new CommandWrapperPane(state));
        }
        titleField = new JTextField(activity.getTitle());
        descriptionField = new JTextArea(activity.getDescription().orElse(null));
        checksPane = new JPanel(new GridBagLayout());
        checksPane.setBorder(BorderFactory.createLineBorder(Color.GREEN, 2));
        commandsPane = new JPanel(new GridBagLayout());
        commandsPane.setBorder(BorderFactory.createLineBorder(Color.BLUE, 2));
        waitingEditWrapperPane = new WaitingEditWrapperPane(state);
        waitingEditWrapperPane.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY, 2));
        Util.putComponentsToVerticalGrid(this,
                -1,
                titleField,
                descriptionField,
                checksPane,
                commandsPane,
                waitingEditWrapperPane);
        fillChecksGrid();
        fillCommandsGrid();
    }

    ActivityEditPane(final State state) {
        this(new Activity(), state);
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
            JOptionPane.showMessageDialog(this, "You cannot delete last check from activity");
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
            JOptionPane.showMessageDialog(this, "You cannot delete last command from activity");
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
        Util.putComponentsToVerticalGrid(checksPane, checks.toArray(Component[]::new));
    }

    private void fillCommandsGrid() {
        Util.putComponentsToVerticalGrid(commandsPane, commands.toArray(Component[]::new));
    }

    @Override
    public Activity get() throws IllegalInputException {
        return new Activity(validateAndGetActivityTitle(),
                descriptionField.getText(),
                validateAndGetChecks(),
                validateAndGetCommands(),
                waitingEditWrapperPane.get());
    }

    private String validateAndGetActivityTitle() throws IllegalInputException {
        final String title = titleField.getText();
        if(title == null || title.isBlank()) {
            titleField.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Activity title cannot be empty");
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
                throw new IllegalInputException("Illegal check");
            }
            next.setBorder(null);
            checkList.add(check);
        }
        if (!Activity.validateChecks(checkList)) {
            checksPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal checks");
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
                throw new IllegalInputException("Illegal command");
            }
            next.setBorder(null);
            commandList.add(command);
        }
        if (!Activity.validateCommands(commandList)) {
            commandsPane.setBorder(BorderFactory.createLineBorder(Color.RED, 2));
            throw new IllegalInputException("Illegal checks");
        }
        commandsPane.setBorder(null);
        return commandList;
    }

    private class CheckWrapperPane extends JPanel implements SupplierThrowing<Check, IllegalInputException> {

        private final CheckEditPane checkEditPane;

        private CheckWrapperPane(final CheckEditPane checkEditPane) {
            this.checkEditPane = checkEditPane;
            Util.putComponentsToVerticalGrid(this, -1, createButtonsPane(), checkEditPane);
        }

        public CheckWrapperPane(final State state) {
            this(new CheckEditPane(state));
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel("Check"));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(e -> addCheckBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(e -> deleteCheck(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(e -> moveCheckUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(e -> moveCheckDown(this));
            return btn;
        }

        @Override
        public Check get() throws IllegalInputException {
            return checkEditPane.get();
        }
    }

    private class CommandWrapperPane extends JPanel implements SupplierThrowing<Command, IllegalInputException> {

        private final CommandEditPane commandEditPane;

        private CommandWrapperPane(final CommandEditPane commandEditPane) {
            this.commandEditPane = commandEditPane;
//            setBorder(BorderFactory.createLineBorder(Color.BLUE));
            Util.putComponentsToVerticalGrid(this, -1, createButtonsPane(), commandEditPane);
        }

        public CommandWrapperPane(final State state) {
            this(new CommandEditPane(state));
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
            panel.add(new JLabel("Command"));
            panel.add(createMoveUpButton());
            panel.add(createMoveDownButton());
            panel.add(createAddButton());
            panel.add(createDeleteButton());
            return panel;
        }

        private Button createAddButton() {
            final Button btn = new Button("+");
            btn.addActionListener(e -> addCommandBelow(this));
            return btn;
        }

        private Button createDeleteButton() {
            final Button btn = new Button("x");
            btn.addActionListener(e -> deleteCommand(this));
            return btn;
        }

        private Button createMoveUpButton() {
            final Button btn = new Button("^");
            btn.addActionListener(e -> moveCommandUp(this));
            return btn;
        }

        private Button createMoveDownButton() {
            final Button btn = new Button("v");
            btn.addActionListener(e -> moveCommandDown(this));
            return btn;
        }

        @Override
        public Command get() throws IllegalInputException{
            return commandEditPane.get();
        }
    }

    private static class WaitingEditWrapperPane extends JPanel implements SupplierThrowing<Waiting, IllegalInputException> {

        private final State state;
        private final JPanel container;
        private WaitingEditPane waitingEditPane;
        private Button addWaitingButton;
        private Button deleteWaitingButton;

        public WaitingEditWrapperPane(final Waiting waiting, final State state) {
            this.state = state;
            this.container = new JPanel();
            Util.putComponentsToVerticalGrid(this,
                    createButtonsPane(),
                    container);
            if (waiting != null) {
                addWaiting(waiting, state);
            } else {
                deleteWaiting();
            }
        }

        public WaitingEditWrapperPane(final State state) {
            this(null, state);
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel();
            addWaitingButton = new Button("+");
            addWaitingButton.addActionListener(e -> addWaiting(new Waiting(), state));
            deleteWaitingButton = new Button("-");
            deleteWaitingButton.addActionListener(e -> deleteWaiting());
            Util.putComponentsToHorizontalGrid(panel,
                    0,
                    new JLabel("Waiting"),
                    addWaitingButton,
                    deleteWaitingButton);
            return panel;
        }

        private void addWaiting(final Waiting waiting, final State state) {
            waitingEditPane = new WaitingEditPane(waiting, state);
            Util.putComponentsToVerticalGrid(container, waitingEditPane);
            addWaitingButton.setVisible(false);
            deleteWaitingButton.setVisible(true);
            revalidate();
        }

        private void deleteWaiting() {
            waitingEditPane = null;
            container.removeAll();
            addWaitingButton.setVisible(true);
            deleteWaitingButton.setVisible(false);
            revalidate();
        }

        @Override
        public Waiting get() throws IllegalInputException {
            return waitingEditPane != null ? waitingEditPane.get() : null;
        }
    }
}
