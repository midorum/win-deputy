package midorum.win32.deputy.ui;

import midorum.win32.deputy.common.CommonUtil;
import midorum.win32.deputy.common.Either;
import midorum.win32.deputy.common.UserActivityObserver;
import midorum.win32.deputy.common.Win32Adapter;
import midorum.win32.deputy.model.Scenario;
import midorum.win32.deputy.model.TaskDispatcher;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.JarURLConnection;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import javax.imageio.ImageIO;
import javax.swing.*;

public class MainForm implements TaskDispatcher {

    private final State state;
    private final JFrame frame;
    private final UiUtil uiUtil;

    public MainForm() {
        final String appVersion = getVersion();
        this.frame = new JFrame("Win Deputy v" + appVersion);

        this.frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.uiUtil = new UiUtil(frame, new Win32Adapter(new UserActivityObserver()));
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        this.frame.setPreferredSize(new Dimension(700, screenSize.height));
        this.state = State.loadState(uiUtil).getOrHandleError(e -> {
            if (!(e instanceof FileNotFoundException)) {
                uiUtil.reportThrowable("Error occurred while loading application state", e);
            }
            return new State(uiUtil);
        });
        setIcon();
        final Logger logger = uiUtil.getLogger();
        logger.info("--------------------------------------------------------");
        logger.info("version: {}", appVersion);
        logger.info("build time: {}", buildTimeString().orElse("N/A"));
        logger.info("java.version: {}", System.getProperty("java.version"));
        logger.info("java.home: {}", System.getProperty("java.home"));
        logger.info("java.vendor: {}", System.getProperty("java.vendor"));
        logger.info("os.name: {}", System.getProperty("os.name"));
        logger.info("os.version: {}", System.getProperty("os.version"));
        logger.info("system architecture: {}", System.getProperty("sun.arch.data.model"));
        logger.info("display mode: {}", GraphicsEnvironment.getLocalGraphicsEnvironment()
                .getDefaultScreenDevice().getDisplayMode());
        logger.info("--------------------------------------------------------");

        logger.info(" name {}", getClass().getName());
        logger.info(" simple name {}", getClass().getSimpleName());
        logger.info(" canonical name {}", getClass().getCanonicalName());
    }

    private void setIcon() {
        try {
            final URL resource = getClass().getResource("/icons/droid-robot.png");
            if (resource != null) this.frame.setIconImage(ImageIO.read(resource));
        } catch (IOException e) {
            uiUtil.logThrowable("Error occurred while loading application icon", e);
        }
    }

    private Optional<String> buildTimeString() {
        final URL resource = getClass().getResource(getClass().getSimpleName() + ".class");
        if (resource != null) {
            final String protocol = resource.getProtocol();
            try {
                if ("file".equals(protocol)) {
                    return Optional.of(CommonUtil.utcTimeString(new File(resource.toURI()).lastModified()));
                } else if ("jar".equals(protocol)) {
                    return Optional.ofNullable(((JarURLConnection) resource.openConnection())
                            .getManifest().getMainAttributes().getValue("Build-Time"));
                }
            } catch (IOException | URISyntaxException e) {
                uiUtil.getLogger().error("cannot obtain build time", e);
            }
        }
        return Optional.empty();
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
        SwingUtil.putComponentsToVerticalGrid(contentPane, 0, form);
        contentPane.revalidate();
    }

    private String getVersion() {
        return getClass().getPackage().getImplementationVersion();
    }

    @Override
    public void createNewScenario() {
        showForm(new ScenarioEditorForm(this, uiUtil));
    }

    @Override
    public void cancelScenarioEditing() {
        resetScenarioViewerForm();
    }

    @Override
    public void endScenarioEditing(final String scenarioPath) {
        loadScenarioPath(scenarioPath, (eitherScenarioOrError, file) ->
                eitherScenarioOrError.consumeOrHandleError((Consumer<? super Scenario>) scenario -> {
                            updateState(file);
                            showForm(new ScenarioViewerForm(this, scenario, file, uiUtil));
                        },
                        e -> {
                            state.getUtilities().reportThrowable("Error occurred while loading scenario", e);
                            showForm(new ScenarioViewerForm(this, uiUtil));
                        }));
    }

    @Override
    public void editScenario(final String scenarioPath) {
        loadScenarioPath(scenarioPath, (eitherScenarioOrError, file) ->
                eitherScenarioOrError.consumeOrHandleError((Consumer<? super Scenario>) scenario ->
                                showForm(new ScenarioEditorForm(this, scenario, file, uiUtil)),
                        e -> state.getUtilities().reportThrowable("Error occurred while loading scenario", e)));
    }

    @Override
    public void loadScenario() {
        askScenarioPath((eitherScenarioOrError, file) ->
                eitherScenarioOrError.consumeOrHandleError((Consumer<? super Scenario>) scenario -> {
                            updateState(file);
                            showForm(new ScenarioViewerForm(this, scenario, file, uiUtil));
                        },
                        e -> {
                            state.getUtilities().reportThrowable("Error occurred while loading scenario", e);
                            showForm(new ScenarioViewerForm(this, uiUtil));
                        }));
    }

    @Override
    public void showCredits() {
        showForm(new CreditsPane(this, uiUtil));
    }

    @Override
    public void closeCredits() {
        resetScenarioViewerForm();
    }

    @Override
    public void closeSettings() {
        resetScenarioViewerForm();
    }

    @Override
    public void showSettings() {
        showForm(new SettingsPane(this, uiUtil));
    }

    private void loadScenarioPath(final String scenarioPath, final BiConsumer<Either<Scenario, IOException>, File> scenarioConsumer) {
        loadScenarioFile(scenarioConsumer, new File(scenarioPath));
    }

    private void askScenarioPath(final BiConsumer<Either<Scenario, IOException>, File> scenarioConsumer) {
        state.getUtilities().askExistFile(state.getWorkingDirectory()).ifPresent(file -> loadScenarioFile(scenarioConsumer, file));
    }

    private void loadScenarioFile(final BiConsumer<Either<Scenario, IOException>, File> scenarioConsumer, final File file) {
        scenarioConsumer.accept(CommonUtil.loadScenario(file), file);
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
            showForm(new ScenarioViewerForm(this, uiUtil));
        } else {
            loadScenarioPath(new File(state.getWorkingDirectory(), state.getScenarioName()).getAbsolutePath(),
                    (eitherScenarioOrError, file) ->
                            eitherScenarioOrError.consumeOrHandleError((Consumer<? super Scenario>) scenario ->
                                            showForm(new ScenarioViewerForm(this, scenario, file, uiUtil)),
                                    e -> {
                                        state.getUtilities().reportThrowable("Error occurred while loading scenario", e);
                                        showForm(new ScenarioViewerForm(this, uiUtil));
                                    }));
        }
    }

}
