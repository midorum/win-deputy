package midorum.win32.deputy.ui;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;

public class Main extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        primaryStage.setTitle("JavaFX App");

        final FileChooser fileChooser = new FileChooser();

        final Label label = new Label();

        final Button button = new Button("Select File");
        button.setOnAction(_ -> {
            File selectedFile = fileChooser.showOpenDialog(primaryStage);
            System.out.println("selectedFile: " + selectedFile);
            label.setText(selectedFile.getAbsolutePath());
        });

        final VBox vBox = new VBox(button, label);
        final Scene scene = new Scene(vBox, 960, 600);

        primaryStage.setScene(scene);
        primaryStage.show();
    }
}