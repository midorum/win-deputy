package midorum.win32.deputy.ui;

import midorum.win32.deputy.model.Activity;
import midorum.win32.deputy.model.Check;
import midorum.win32.deputy.model.Command;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

class ActivityEditPane extends JPanel implements Supplier<Activity> {

    private final State state;
    private JTextField titleField;
    private JTextArea descriptionField;
    private List<CheckWrapperPane> checks;
    private List<CommandWrapperPane> commands;
    private JPanel checksPane;
    private JPanel commandsPane;

    ActivityEditPane(final Activity activity, final State state) {
        this.state = state;
        checks = new ArrayList<>();
        checks.addAll(activity.getChecks().stream()
                .map(check -> new CheckEditPane(check, state))
                .map(CheckWrapperPane::new)
                .collect(Collectors.toCollection(ArrayList::new)));
        commands = new ArrayList<>();
        commands.addAll(activity.getCommands().stream()
                .map(command -> new CommandEditPane(command, state))
                .map(CommandWrapperPane::new)
                .collect(Collectors.toCollection(ArrayList::new)));

        compose(activity.getTitle(), activity.getDescription());
    }

    ActivityEditPane(final State state) {
        this.state = state;
        checks = new ArrayList<>();
        checks.add(new CheckWrapperPane(state));
        commands = new ArrayList<>();
        commands.add(new CommandWrapperPane(state));

        compose(null, null);
    }

    private void compose(final String title, final String description) {
//        setBorder(BorderFactory.createLineBorder(Color.ORANGE));
        titleField = new JTextField(title);
        descriptionField = new JTextArea(description);
        checksPane = new JPanel(new GridBagLayout());
//        checksPane.setBorder(BorderFactory.createLineBorder(Color.BLUE));
        commandsPane = new JPanel(new GridBagLayout());
//        commandsPane.setBorder(BorderFactory.createLineBorder(Color.GREEN));
        Util.putComponentsToVerticalGrid(this, -1, titleField, descriptionField, checksPane, commandsPane);
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
    public Activity get() {
        return new Activity(titleField.getText(),
                descriptionField.getText(),
                checks.stream().map(CheckWrapperPane::get).toList(),
                commands.stream().map(CommandWrapperPane::get).toList(),
                null);
    }

    private class CheckWrapperPane extends JPanel implements Supplier<Check> {

        private final CheckEditPane checkEditPane;

        private CheckWrapperPane(final CheckEditPane checkEditPane) {
            this.checkEditPane = checkEditPane;
//            setBorder(BorderFactory.createLineBorder(Color.BLUE));
            Util.putComponentsToVerticalGrid(this, -1, createButtonsPane(), checkEditPane);
        }

        public CheckWrapperPane(final State state) {
            this(new CheckEditPane(state));
        }

        private JPanel createButtonsPane() {
            final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            panel.setSize(panel.getPreferredSize());
//            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
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
        public Check get() {
            return checkEditPane.get();
        }
    }

    private class CommandWrapperPane extends JPanel implements Supplier<Command> {

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
//            panel.setBorder(BorderFactory.createLineBorder(Color.RED));
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
        public Command get() {
            return commandEditPane.get();
        }
    }
}
