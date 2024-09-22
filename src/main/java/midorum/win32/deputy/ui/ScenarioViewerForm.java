package midorum.win32.deputy.ui;

import com.midorum.win32api.hook.GlobalKeyHook;
import com.midorum.win32api.hook.KeyHookHelper;
import com.midorum.win32api.win32.Win32VirtualKey;
import dma.validation.Validator;
import midorum.win32.deputy.common.UserActivityObserver;
import midorum.win32.deputy.executor.ExecutorImpl;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

public class ScenarioViewerForm extends JPanel implements Displayable {

    private final Logger executorLogger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private static final int PANE_MARGIN = 10;
    private final TaskDispatcher taskDispatcher;
    private final State state;
    private final JLabel scenarioPathLabel;
    private Button editScenarioButton;
    private Button runScenarioButton;
    private Button stopScenarioButton;
    private final JLabel scenarioTitleLabel;
    private final JLabel scenarioDescriptionLabel;
    private Scenario scenario;
    private final ExecutorImpl executor;
    private Button loadScenarioButton;
    private Button createScenarioButton;
    private final AtomicReference<GlobalKeyHook> cancelKeyHook = new AtomicReference<>();
    private UserActivityObserver userActivityObserver;

    ScenarioViewerForm(final TaskDispatcher taskDispatcher) {
        this.taskDispatcher = Validator.checkNotNull(taskDispatcher).orThrowForSymbol("taskDispatcher");
        this.state = new State(new UiUtil(this));
        this.userActivityObserver = new UserActivityObserver();
        this.executor = new ExecutorImpl(userActivityObserver);
        this.scenarioPathLabel = new JLabel();
        this.scenarioTitleLabel = new JLabel();
        this.scenarioDescriptionLabel = new JLabel();
        SwingUtil.putComponentsToVerticalGrid(this,
                3,
                scenarioPathLabel,
                scenarioTitleLabel,
                scenarioDescriptionLabel,
                new JPanel(),
                createButtonPane());
        scenarioPathLabel.setText("New scenario");
        editScenarioButton.setEnabled(false);
        runScenarioButton.setEnabled(false);
        stopScenarioButton.setEnabled(false);
    }

    ScenarioViewerForm(final TaskDispatcher taskDispatcher, final Scenario scenario, final File scenarioFile) {
        this(taskDispatcher);
        this.scenario = scenario;
        state.setWorkingDirectory(scenarioFile.getParentFile());
        state.setScenarioName(scenarioFile.getName());
        scenarioPathLabel.setText(scenarioFile.getAbsolutePath());
        scenarioTitleLabel.setText(scenario.getTitle());
        scenario.getDescription().ifPresent(scenarioDescriptionLabel::setText);
        editScenarioButton.setEnabled(true);
        runScenarioButton.setEnabled(true);
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        loadScenarioButton = createLoadScenarioButton();
        buttonPane.add(loadScenarioButton);
        createScenarioButton = createCreateScenarioButton();
        buttonPane.add(createScenarioButton);
        editScenarioButton = createEditScenarioButton();
        buttonPane.add(editScenarioButton);
        runScenarioButton = createRunScenarioButton();
        buttonPane.add(runScenarioButton);
        stopScenarioButton = createStopScenarioButton();
        buttonPane.add(stopScenarioButton);
        return buttonPane;
    }

    private Button createLoadScenarioButton() {
        final Button btn = new Button("Load scenario");
        btn.addActionListener(e -> taskDispatcher.loadScenario());
        return btn;
    }

    private Button createEditScenarioButton() {
        final Button btn = new Button("Edit scenario");
        btn.addActionListener(e -> taskDispatcher.editScenario(new File(state.getWorkingDirectory(),
                state.getScenarioName()).getAbsolutePath()));
        return btn;
    }

    private Button createCreateScenarioButton() {
        final Button btn = new Button("Create scenario");
        btn.addActionListener(e -> taskDispatcher.createNewScenario());
        return btn;
    }

    private Button createRunScenarioButton() {
        final Button btn = new Button("Run scenario");
        btn.addActionListener(e -> {
            if (scenario != null) {
                lockForm();
                final GlobalKeyHook oldHook = cancelKeyHook.getAndSet(setCancelTaskHook2());
                if (oldHook != null) oldHook.unhook();
                executor.sendRoutineTask(state.getWorkingDirectory(), scenario, throwable -> {
                    if (throwable instanceof UserMessageException ex) {
                        state.getUtilities().reportThrowable(ex.getMessage(), ex);
                    } else if (throwable instanceof ControlledInterruptedException ex) {
                        state.getUtilities().reportThrowable("Scenario executing has been interrupted", ex);
                    } else {
                        state.getUtilities().reportThrowable("Error occurred while executing scenario", throwable);
                    }
                    unlockForm();
                });
            }
        });
        return btn;
    }

    private Button createStopScenarioButton() {
        final Button btn = new Button("Stop scenario");
        btn.addActionListener(e -> {
            executor.cancelCurrentTask();
            final GlobalKeyHook cancelKeyHook = this.cancelKeyHook.get();
            if (cancelKeyHook != null) {
                cancelKeyHook.unhook();
            }
            unlockForm();
        });
        return btn;
    }

    private void lockForm() {
        loadScenarioButton.setEnabled(false);
        editScenarioButton.setEnabled(false);
        createScenarioButton.setEnabled(false);
        runScenarioButton.setEnabled(false);
        stopScenarioButton.setEnabled(true);
    }

    private void unlockForm() {
        loadScenarioButton.setEnabled(true);
        editScenarioButton.setEnabled(true);
        createScenarioButton.setEnabled(true);
        runScenarioButton.setEnabled(true);
        stopScenarioButton.setEnabled(false);
    }

    private GlobalKeyHook setCancelTaskHook() {
        return KeyHookHelper.getInstance().setGlobalHook(
                new KeyHookHelper.KeyEventBuilder().virtualKey(Win32VirtualKey.VK_S).withControl().withShift().build(),
                KeyHookHelper.KeyEventComparator.byAltControlShiftCode,
                keyEvent -> {
                    final boolean result = executor.cancelCurrentTask();
                    if (result)
                        unlockForm();
                    return result;// release the hook when task cancelled
                }, throwable -> {
                    state.getUtilities().logThrowable("error occurred while cancel executing task", throwable);
                    return true; // release hook
                });
    }

    private GlobalKeyHook setCancelTaskHook2() {
        final GlobalKeyHook.KeyEvent stopKey = new KeyHookHelper.KeyEventBuilder()
                .virtualKey(Win32VirtualKey.VK_S).withControl().withShift().build();
        final KeyHookHelper keyHookHelper = KeyHookHelper.getInstance();
        return keyHookHelper.setGlobalHook(
                keyEvent -> {
                    executorLogger.trace(() -> ">>> GlobalHook: keyEvent: " + keyEvent + " " + keyEvent.toPrettyString());
                    userActivityObserver.checkKeyEvent(keyEvent);
                    final boolean isNeedStop = KeyHookHelper.KeyEventComparator.byAltControlShiftCode.compare(stopKey, keyEvent) == 0;
                    if (isNeedStop && executor.cancelCurrentTask()) {
                        unlockForm();
                    }
                    return isNeedStop ? KeyHookHelper.Result.deleteHook : KeyHookHelper.Result.propagateEvent;
                },
                throwable -> {
                    state.getUtilities().logThrowable("error occurred while cancel executing task", throwable);
                    return KeyHookHelper.Result.keepHook;
                }
        );
    }

    @Override
    public void display() {
        display(null);
    }

    @Override
    public void display(final String source) {
        setVisible(true);
    }

    @Override
    public void conceal() {
        setVisible(false);
    }

}
