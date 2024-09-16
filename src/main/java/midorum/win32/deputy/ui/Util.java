package midorum.win32.deputy.ui;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import com.midorum.win32api.struct.PointInt;
import midorum.win32.deputy.common.Either;
import midorum.win32.deputy.model.Scenario;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class Util {

    public static final Pattern POINT_PATTERN = Pattern.compile("^\\((\\d+),(\\d+)\\)$");

    private Util() {
    }

    public static void retainComponentSize(final Component component) {
        final Dimension maximumSize = component.getMaximumSize();
        final Dimension preferredSize = component.getPreferredSize();
        preferredSize.setSize(maximumSize.getWidth(), preferredSize.getHeight());
        component.setMaximumSize(preferredSize);
    }

    public static <T> int getIndex(T[] array, T t) {
        for (int i = 0; i < array.length; i++)
            if (Objects.equals(array[i], t)) return i;
        return -1;
    }

    public static void putComponentsToVerticalGrid(final Container container, final double[] weights, final Component... components) {
        if (container == null || weights == null || components == null || weights.length != components.length)
            throw new IllegalArgumentException();
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
//        c.anchor = GridBagConstraints.FIRST_LINE_START; // align to the top left
        c.anchor = GridBagConstraints.PAGE_START; // align to the top
        c.weightx = 0.5; // fill all available width
        c.gridy = 0; // first grid row
        for (int i = 0; i < components.length; i++) {
            if (weights[i] > 0.0) {
//                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.fill = GridBagConstraints.BOTH; // resize component in both directions
                c.weighty = weights[i]; // fill all available height
            } else {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weighty = 0.0; // do not fill all available height
            }
            container.add(components[i], c);
            c.gridy++; // increment grid row
        }
    }

    public static void putComponentsToVerticalGrid(final Container container, int stretch, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
//        c.anchor = GridBagConstraints.FIRST_LINE_START; // align to the top left
        c.anchor = GridBagConstraints.PAGE_START; // align to the top
        c.weightx = 0.5; // fill all available width
        c.gridy = 0; // first grid row
        final int stretchIndex = stretch == Integer.MAX_VALUE ? components.length - 1 : stretch;
        for (int i = 0; i < components.length; i++) {
            if (i == stretchIndex) {
                c.fill = GridBagConstraints.BOTH; // resize component in both directions
                c.weighty = 0.5; // fill all available height
            } else if (stretchIndex == -1 && i == components.length - 1) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weighty = 0.5; // fill all available height
            } else {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weighty = 0.0; // do not fill all available height
            }
            container.add(components[i], c);
            c.gridy++; // increment grid row
        }
    }

    public static void putComponentsToVerticalGrid(final Container container, final Component... components) {
        putComponentsToVerticalGrid(container, Integer.MAX_VALUE, components);
    }

    public static void putComponentsToHorizontalGrid(final Container container, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weightx = 0.0; // do not fill all available width
        c.weighty = 0.0; // do not fill all available height
        for (int i = 0; i < components.length; i++) {
            if (i == components.length - 1) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weightx = 0.5; // fill all available width
            }
            container.add(components[i], c);
            c.gridx++; // increment grid column
        }
    }

    public static void putComponentsToHorizontalGrid(final Container container, final int stretch, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weighty = 0.0; // do not fill all available height
        final int stretchIndex = stretch == Integer.MAX_VALUE ? components.length - 1 : stretch;
        for (int i = 0; i < components.length; i++) {
            if (i == stretchIndex) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weightx = 0.5; // fill all available width
            } else {
                c.fill = GridBagConstraints.NONE; // do not resize component
                c.weightx = 0.0; // do not fill all available width
            }
            container.add(components[i], c);
            c.gridx++; // increment grid column
        }
    }

    public static void putComponentsToHorizontalGrid(final Container container, final double[] weights, final Component... components) {
        if (container == null || weights == null || components == null || weights.length != components.length)
            throw new IllegalArgumentException();
        if (!(container.getLayout() instanceof GridBagLayout))
            container.setLayout(new GridBagLayout());
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weighty = 0.0; // do not fill all available height
        for (int i = 0; i < components.length; i++) {
            if (weights[i] > 0.0) {
                c.fill = GridBagConstraints.HORIZONTAL; // resize component horizontally
                c.weightx = weights[i]; // fill all available width
            } else {
                c.fill = GridBagConstraints.NONE; // do not resize component
                c.weightx = 0.0; // do not fill all available width
            }
            container.add(components[i], c);
            c.gridx++; // increment grid column
        }
    }

    public static Optional<File> askFile(final File currentDirectory, final Component parent, final boolean shouldExist) {
        final JFileChooser fileChooser = new JFileChooser();
        fileChooser.setCurrentDirectory(currentDirectory);
        if (fileChooser.showOpenDialog(parent) == JFileChooser.APPROVE_OPTION) {
            final File selectedFile = fileChooser.getSelectedFile();
            if (shouldExist) {
                if (selectedFile.exists()) return Optional.of(selectedFile);
            } else {
                if (!selectedFile.exists()
                        || JOptionPane.showConfirmDialog(parent,
                        "Do you want to replace existing file?",
                        "Warning",
                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    return Optional.of(new File(getWorkingDirectoryForPath(selectedFile.getParentFile()), selectedFile.getName()));
                } else {
                    return Optional.of(new File(getWorkingDirectoryForPath(selectedFile.getParentFile()), getDefaultFileName()));
                }
            }
        }
        return Optional.empty();
    }

    public static File getWorkingDirectoryForPath(final File file) {
        final String path = file.getAbsolutePath();
        return path.endsWith("shots") ? file.getParentFile() : file;
    }

    public static Optional<File> askNewFile(final File currentDirectory, final Component parent) {
        return askFile(currentDirectory, parent, false);
    }

    public static Optional<File> askExistFile(final File currentDirectory, final Component parent) {
        return askFile(currentDirectory, parent, true);
    }

    public static void askNewFileAndUpdateState(final State state, final Consumer<File> fileConsumer, final Component parent) {
        Util.askNewFile(state.getWorkingDirectory(), parent).ifPresent(file -> {
            final File workingDirectory = state.getWorkingDirectory();
            final File selectedDirectory = file.getParentFile();
            if (workingDirectory == null) {
                state.setWorkingDirectory(selectedDirectory);
            } else if (!Objects.equals(workingDirectory, selectedDirectory)) {
                if (JOptionPane.showConfirmDialog(parent,
                        "Your selected directory does not match current working directory." +
                                " It is not recommended to change working directory." +
                                " Would you like to change working directory anyway?",
                        "Warning",
                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                    state.setWorkingDirectory(selectedDirectory);
                }
            }
            fileConsumer.accept(file);
        });
    }

    public static String getPathForImages(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() + File.separator + "shots" : "shots";
    }

    public static String getPathForScenarios(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() : "";
    }

    public static String getDefaultFileName() {
        return "_" + System.currentTimeMillis();
    }

    public static Either<File, IOException> saveImage(final BufferedImage bufferedImage, final String path, final String name) {
        return Either.value(() -> new FileServiceProvider()
                        .withFile(path, name, "png")
                        .writeImage(bufferedImage))
                .orException(() -> new IOException("error while saving image: " + path + File.separator + name));
    }

    public static Either<File, IOException> saveScenario(final Scenario scenario, final String path, final String name) {
        return Either.value(() -> new FileServiceProvider()
                        .withFile(path, name, "wds")
                        .writeJson(scenario))
                .orException(() -> new IOException("error while saving scenario: " + path + File.separator + name));
    }

    public static Either<Scenario, IOException> loadScenario(final File file) {
        final ObjectMapper objectMapper = new ObjectMapper()
                .registerModule(new ParameterNamesModule())
                .registerModule(new Jdk8Module())
                .registerModule(new JavaTimeModule());
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        return Either.value(() -> objectMapper.readValue(file, Scenario.class))
                .orException(() -> new IOException("error while loading scenario: " + file.getAbsolutePath()));
    }

    public static String pointToString(final PointInt point) {
        return "(" + point.x() + "," + point.y() + ")";
    }

    public static Optional<PointInt> stringToPoint(final String s) {
        if (s == null) return Optional.empty();
        final Matcher matcher = POINT_PATTERN.matcher(s);
        if (!matcher.matches()) return Optional.empty();
        return Optional.of(new PointInt(Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2))));
    }

}
