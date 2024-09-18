package midorum.win32.deputy.ui;

import com.midorum.win32api.hook.GlobalKeyHook;
import com.midorum.win32api.hook.KeyHookHelper;
import com.midorum.win32api.win32.Win32VirtualKey;
import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.model.Scenario;
import midorum.win32.deputy.model.TaskDispatcher;

import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import javax.swing.*;

public class MainForm implements TaskDispatcher {

    private final State state;
    private final JFrame frame;

    public MainForm() {
        this.frame = new JFrame("Win Deputy");
        this.frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.frame.setPreferredSize(new Dimension(700, 500));
        final UiUtil utilities = new UiUtil(frame);
        this.state = State.loadState(utilities).getOrHandleError(e -> {
            if (!(e instanceof FileNotFoundException)) {
                utilities.reportThrowable("Error occurred while loading application state", e);
            }
            return new State(utilities);
        });
    }

    public static void main(String[] args) {
        new MainForm().display();
    }

    public void display() {
        resetScenarioViewerForm();
        this.frame.pack();
        this.frame.setVisible(true);
    }

    private void showForm(final Component form) {
        final Container contentPane = frame.getContentPane();
        contentPane.removeAll();
        SwingUtil.putComponentsToVerticalGrid(contentPane, form);
        contentPane.revalidate();
    }

    @Override
    public void createNewScenario() {
        showForm(new ScenarioEditorForm(this));
    }

    @Override
    public void cancelScenarioEditing() {
        resetScenarioViewerForm();
    }

    @Override
    public void endScenarioEditing(final String scenarioPath) {
        loadScenarioPath(scenarioPath, (scenario, file) -> {
            updateState(file);
            showForm(new ScenarioViewerForm(this, scenario, file));
        });
    }

    @Override
    public void editScenario(final String scenarioPath) {
        loadScenarioPath(scenarioPath, (scenario, file) ->
                showForm(new ScenarioEditorForm(this, scenario, file)));
    }

    @Override
    public void loadScenario() {
        askScenarioPath((scenario, file) -> {
            updateState(file);
            showForm(new ScenarioViewerForm(this, scenario, file));
        });
    }

    private void loadScenarioPath(final String scenarioPath, final BiConsumer<Scenario, File> scenarioConsumer) {
        loadScenarioFile(scenarioConsumer, new File(scenarioPath));
    }

    private void askScenarioPath(final BiConsumer<Scenario, File> scenarioConsumer) {
        state.getUtilities().askExistFile(state.getWorkingDirectory()).ifPresent(file -> loadScenarioFile(scenarioConsumer, file));
    }

    private void loadScenarioFile(final BiConsumer<Scenario, File> scenarioConsumer, final File file) {
        CommonUtil.loadScenario(file).consumeOrHandleError((Consumer<? super Scenario>) scenario ->
                        scenarioConsumer.accept(scenario, file),
                e -> state.getUtilities().reportThrowable("Error occurred while loading scenario", e));
    }

    private void updateState(final File file) {
        state.setWorkingDirectory(file.getParentFile());
        state.setScenarioName(file.getName());
        try {
            state.storeToFile();
        } catch (IOException e) {
            state.getUtilities().reportThrowable("Error occurred while saving application state", e);
        }
    }

    private void resetScenarioViewerForm() {
        if (state.getWorkingDirectory() == null || state.getScenarioName() == null) {
            showForm(new ScenarioViewerForm(this));
        } else {
            loadScenarioPath(new File(state.getWorkingDirectory(), state.getScenarioName()).getAbsolutePath(),
                    (scenario, file) -> showForm(new ScenarioViewerForm(this, scenario, file)));
        }
    }

}
