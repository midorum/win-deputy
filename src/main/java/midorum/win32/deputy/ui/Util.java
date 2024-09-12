package midorum.win32.deputy.ui;

import midorum.win32.deputy.common.Either;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

final class Util {
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

    public static void putComponentsToVerticalGrid(final Container container, int stretch, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.FIRST_LINE_START; // align to the top left
        c.weightx = 0.5; // fill all available width
        c.gridy = 0; // first grid row
        final int stretchIndex = stretch == Integer.MAX_VALUE ? components.length - 1 : stretch;
        for (int i = 0; i < components.length; i++) {
            if (i == stretchIndex) {
                c.fill = GridBagConstraints.BOTH; // resize component in both directions
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

    public static void putComponentsToHorizontalGridStretchFirst(final Container container, final Component... components) {
        if (!(container.getLayout() instanceof GridBagLayout)) {
            container.setLayout(new GridBagLayout());
        }
        final GridBagConstraints c = new GridBagConstraints();
        c.anchor = GridBagConstraints.LINE_START; // align to left
        c.gridx = 0; // first grid column
        c.weighty = 0.0; // do not fill all available height
        for (int i = 0; i < components.length; i++) {
            if (i == 0) {
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

    public static String getPathForImages(final File workingDirectory) {
        return workingDirectory != null ? workingDirectory.getAbsolutePath() + File.separator + "shots" : "shots";
    }

    public static String getDefaultFileName() {
        return "_" + System.currentTimeMillis();
    }

    public static void captureAndSaveRegion(final State state,
                                            final Component parent,
                                            final Consumer<File> successFileConsumer,
                                            final Consumer<IOException> errorConsumer) {
        new CaptureWindow(maybeImage -> maybeImage.ifPresentOrElse(image ->
                        askNewFile(state.getWorkingDirectory(), parent).ifPresent(file -> {
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
                            saveImage(image, getPathForImages(state.getWorkingDirectory()), file.getName())
                                    .consumeOrHandleError(successFileConsumer, errorConsumer);
                        }),
                () -> JOptionPane.showMessageDialog(parent, "No rectangle was captured"))).display();
    }

    private static Either<File, IOException> saveImage(final BufferedImage bufferedImage, final String path, final String name) {
        return Either.value(() -> new FileServiceProvider()
                        .withFile(path, name, "png")
                        .writeImage(bufferedImage))
                .orException(() -> new IOException("error while saving image: " + path + File.separator + name));
    }

    public static void pickFile(final State state, final Component parent, final Consumer<File> successFileConsumer) {
        final BiConsumer<File, File> updateStateAndDoJob = (workingDirectory, acceptedFile) -> {
            state.setWorkingDirectory(workingDirectory);
            successFileConsumer.accept(acceptedFile);
        };
        Util.askExistFile(state.getWorkingDirectory(), parent).ifPresent(file -> {
            final File workingDirectory = state.getWorkingDirectory();
            final File selectedWorkingDirectory = Util.getWorkingDirectoryForPath(file.getParentFile());
            if (workingDirectory == null) {
                updateStateAndDoJob.accept(selectedWorkingDirectory, file);
                return;
            }
            if (Objects.equals(workingDirectory, selectedWorkingDirectory)) {
                successFileConsumer.accept(file);
            } else if (JOptionPane.showConfirmDialog(parent,
                    "Your selected directory does not match current working directory." +
                            " It is not recommended to change working directory." +
                            " Would you like to change working directory anyway?",
                    "Warning",
                    JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
                updateStateAndDoJob.accept(selectedWorkingDirectory, file);
            }
        });
    }

}
