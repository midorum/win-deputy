package midorum.win32.deputy.ui;

import com.midorum.win32api.hook.GlobalKeyHook;
import com.midorum.win32api.hook.KeyHookHelper;
import com.midorum.win32api.win32.Win32VirtualKey;
import dma.validation.Validator;
import midorum.win32.deputy.common.GuardedWin32Adapter;
import midorum.win32.deputy.common.UserActivityObserver;
import midorum.win32.deputy.executor.ExecutorImpl;
import midorum.win32.deputy.i18n.UiElement;
import midorum.win32.deputy.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicReference;

public class ScenarioViewerForm extends JPanel implements Displayable {

    private static final int PANE_MARGIN = 10;
    private final Logger executorLogger = LogManager.getLogger(IExecutor.LOGGER_NAME);
    private final TaskDispatcher taskDispatcher;
    private final State state;
    private final ExecutorImpl executor;
    private final AtomicReference<GlobalKeyHook> cancelKeyHook = new AtomicReference<>();
    private final UserActivityObserver userActivityObserver;
    private final JLabel scenarioPathLabel;
    private final JLabel scenarioTitleLabel;
    private final JLabel scenarioDescriptionLabel;
    private final Button editScenarioButton;
    private final Button runScenarioButton;
    private final Button stopScenarioButton;
    private final Button loadScenarioButtonCentral;
    private final Button loadScenarioButtonBottom;
    private final Button createScenarioButton;
    private final Button showSettingsButton;
    private Scenario scenario;

    ScenarioViewerForm(final TaskDispatcher taskDispatcher,
                       final UiUtil uiUtil,
                       final Settings settings) {
        this.taskDispatcher = Validator.checkNotNull(taskDispatcher).orThrowForSymbol("taskDispatcher");
        this.state = new State(uiUtil);
        this.userActivityObserver = UserActivityObserver.getInstance();
        this.executor = new ExecutorImpl(userActivityObserver, new GuardedWin32Adapter(userActivityObserver), settings);
        this.scenarioPathLabel = new JLabel();
        this.scenarioTitleLabel = new JLabel();
        this.scenarioDescriptionLabel = new JLabel();
        this.loadScenarioButtonCentral = createLoadScenarioButton();
        this.loadScenarioButtonBottom = createLoadScenarioButton();
        this.createScenarioButton = createCreateScenarioButton();
        this.editScenarioButton = createEditScenarioButton();
        this.runScenarioButton = createRunScenarioButton();
        this.stopScenarioButton = createStopScenarioButton();
        this.showSettingsButton = createShowSettingsButton();
        SwingUtil.putComponentsToVerticalGrid(this,
                0,
                createCentralPane(),
                createButtonPane());
        scenarioPathLabel.setText(UiElement.scenarioIsNotLoadedLabel.forUserLocale());
        editScenarioButton.setEnabled(false);
        runScenarioButton.setEnabled(false);
        stopScenarioButton.setEnabled(false);
        showCentralButton(false);
    }

    ScenarioViewerForm(final TaskDispatcher taskDispatcher,
                       final Scenario scenario,
                       final File scenarioFile,
                       final UiUtil uiUtil,
                       final Settings settings) {
        this(taskDispatcher, uiUtil, settings);
        this.scenario = scenario;
        state.setWorkingDirectory(scenarioFile.getParentFile());
        state.setScenarioName(scenarioFile.getName());
        scenarioPathLabel.setText(scenarioFile.getAbsolutePath());
        scenarioTitleLabel.setText(scenario.getTitle());
        scenario.getDescription().ifPresent(text -> scenarioDescriptionLabel.setText("<html><p>" + text + "</p></html>")); // word wrap hack
        editScenarioButton.setEnabled(true);
        runScenarioButton.setEnabled(true);
        showCentralButton(false);
    }

    private JPanel createCentralPane() {
        final Font font = scenarioTitleLabel.getFont();
        final Font enlargedFont = new Font(font.getName(), Font.BOLD, 30);
        final Font middleFont = new Font(font.getName(), Font.PLAIN, 18);
        final JLabel currentScenarioLabel = new JLabel(UiElement.currentScenarioLabel.forUserLocale());
        currentScenarioLabel.setFont(middleFont);
        currentScenarioLabel.setHorizontalAlignment(SwingConstants.CENTER);
        scenarioTitleLabel.setFont(enlargedFont);
        scenarioTitleLabel.setHorizontalAlignment(SwingConstants.CENTER);
        scenarioDescriptionLabel.setFont(middleFont);
        scenarioDescriptionLabel.setHorizontalAlignment(SwingConstants.CENTER);
        scenarioPathLabel.setFont(middleFont);
        scenarioPathLabel.setHorizontalAlignment(SwingConstants.CENTER);
        runScenarioButton.setForeground(Color.GREEN);
        stopScenarioButton.setForeground(Color.RED);
        final JPanel panel = new JPanel();
        panel.setFont(enlargedFont); // for panel buttons
        panel.setLayout(new GridBagLayout());
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 0.5;
        c.gridy = 0;
        c.weighty = 0.3;
        panel.add(new JPanel(), c); // stub
        c.gridy++;
        c.weighty = 0.0;
        panel.add(currentScenarioLabel, c);
        c.gridy++;
        panel.add(scenarioTitleLabel, c);
        c.gridy++;
        panel.add(scenarioDescriptionLabel, c);
        c.gridy++;
        panel.add(scenarioPathLabel, c);
        c.fill = GridBagConstraints.NONE;
        c.gridy++;
        c.weighty = 0.5;
        panel.add(loadScenarioButtonCentral, c);
        c.gridy++;
        panel.add(runScenarioButton, c);
        c.gridy++;
        panel.add(stopScenarioButton, c);
        return panel;
    }

    private JPanel createButtonPane() {
        final JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
        buttonPane.setBorder(BorderFactory.createEmptyBorder(0, PANE_MARGIN, PANE_MARGIN, PANE_MARGIN));
        buttonPane.add(Box.createHorizontalGlue());
        buttonPane.add(loadScenarioButtonBottom);
        buttonPane.add(createScenarioButton);
        buttonPane.add(editScenarioButton);
        buttonPane.add(showSettingsButton);
        buttonPane.add(openLogsButton());
        buttonPane.add(createShowAboutButton());
        return buttonPane;
    }

    private Button createLoadScenarioButton() {
        final Button btn = new Button(UiElement.loadScenarioButtonText.forUserLocale());
        btn.addActionListener(_ -> taskDispatcher.loadScenario());
        return btn;
    }

    private Button createEditScenarioButton() {
        final Button btn = new Button(UiElement.editScenarioButtonText.forUserLocale());
        btn.addActionListener(_ -> taskDispatcher.editScenario(new File(state.getWorkingDirectory(),
                state.getScenarioName()).getAbsolutePath()));
        return btn;
    }

    private Button createCreateScenarioButton() {
        final Button btn = new Button(UiElement.createScenarioButtonText.forUserLocale());
        btn.addActionListener(_ -> taskDispatcher.createNewScenario());
        return btn;
    }

    private Button createRunScenarioButton() {
        final Button btn = new Button(UiElement.runScenarioButtonText.forUserLocale());
        btn.addActionListener(_ -> {
            if (scenario != null) {
                lockForm();
                final GlobalKeyHook oldHook = cancelKeyHook.getAndSet(setCancelTaskHook());
                if (oldHook != null) oldHook.unhook();
                executor.sendRoutineTask(state.getWorkingDirectory(), scenario, throwable -> {
                    if (throwable instanceof UserMessageException ex) {
                        state.getUtilities().reportThrowable(ex, ex.getUiElement(), ex.getArgs());
                    } else if (throwable instanceof ControlledInterruptedException ex) {
                        state.getUtilities().reportThrowable(ex, UiElement.scenarioExecutingInterrupted);
                    } else {
                        state.getUtilities().reportThrowable(throwable, UiElement.errorOccurredWhileExecutingScenario);
                    }
                    unhookAndUnlockForm();
                });
            }
        });
        return btn;
    }

    private Button createStopScenarioButton() {
        final Button btn = new Button(UiElement.stopScenarioButtonText.forUserLocale());
        btn.addActionListener(_ -> {
            executor.cancelCurrentTask();
            unhookAndUnlockForm();
        });
        return btn;
    }

    private Button createShowSettingsButton() {
        final Button btn = new Button(UiElement.settingsButtonText.forUserLocale());
        btn.addActionListener(_ -> taskDispatcher.showSettings());
        return btn;
    }

    private Button openLogsButton() {
        final Button btn = new Button(UiElement.logsButtonText.forUserLocale());
        btn.addActionListener(_ -> {
            try {
                Desktop.getDesktop().open(new File("logs"));
            } catch (IOException ex) {
                state.getUtilities().reportThrowable(ex, UiElement.cannotOpenLogsLocation);
            }
        });
        return btn;
    }

    private Button createShowAboutButton() {
        final Button btn = new Button(UiElement.aboutButtonText.forUserLocale());
        btn.addActionListener(_ -> taskDispatcher.showCredits());
        return btn;
    }

    private void showCentralButton(boolean isRunning) {
        loadScenarioButtonCentral.setVisible(this.scenario == null);
        runScenarioButton.setVisible(this.scenario != null && !isRunning);
        stopScenarioButton.setVisible(this.scenario != null && isRunning);
        revalidate();
    }

    private void lockForm() {
        loadScenarioButtonCentral.setEnabled(false);
        loadScenarioButtonBottom.setEnabled(false);
        editScenarioButton.setEnabled(false);
        createScenarioButton.setEnabled(false);
        runScenarioButton.setEnabled(false);
        stopScenarioButton.setEnabled(true);
        showSettingsButton.setEnabled(false);
        showCentralButton(true);
    }

    private void unlockForm() {
        loadScenarioButtonCentral.setEnabled(true);
        loadScenarioButtonBottom.setEnabled(true);
        editScenarioButton.setEnabled(true);
        createScenarioButton.setEnabled(true);
        runScenarioButton.setEnabled(true);
        stopScenarioButton.setEnabled(false);
        showSettingsButton.setEnabled(true);
        showCentralButton(false);
    }

    private void unhookAndUnlockForm() {
        final GlobalKeyHook cancelKeyHook = this.cancelKeyHook.get();
        if (cancelKeyHook != null) {
            cancelKeyHook.unhook();
        }
        unlockForm();
    }

    private GlobalKeyHook setCancelTaskHook() {
        final GlobalKeyHook.KeyEvent stopKey = new KeyHookHelper.KeyEventBuilder()
                .virtualKey(Win32VirtualKey.VK_S).withControl().withShift().build();
        final KeyHookHelper keyHookHelper = KeyHookHelper.getInstance();
        final String hookId = Long.toString(System.currentTimeMillis());
        return keyHookHelper.setGlobalHook(hookId,
                keyEvent -> {
                    executorLogger.trace(() -> "GlobalKeyHook[" + hookId + "] keyEvent: " + keyEvent
                            + " " + keyEvent.toPrettyString());
                    userActivityObserver.checkKeyEvent(keyEvent);
                    final boolean isNeedStop = KeyHookHelper.KeyEventComparator.byAltControlShiftCode
                            .compare(stopKey, keyEvent) == 0;
                    if (isNeedStop && executor.cancelCurrentTask()) {
                        unlockForm();
                    }
                    return isNeedStop ? KeyHookHelper.Result.deleteHook : KeyHookHelper.Result.propagateEvent;
                },
                throwable -> {
                    state.getUtilities().logThrowable(throwable, "error occurred while cancel executing task");
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
