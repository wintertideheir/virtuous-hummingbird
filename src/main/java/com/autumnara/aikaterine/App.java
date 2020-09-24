package com.autumnara.aikaterine;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Accordion;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;

public class App extends Application{

    @Override
    public void start(Stage stage)
    throws IOException {
        URL fxml = getClass().getResource("/aikaterine.fxml");
        FXMLLoader fxmlLoader = new FXMLLoader(fxml);
        Accordion root = (Accordion) fxmlLoader.load();
        Scene scene = new Scene(root);

        stage.setScene(scene);
        stage.setTitle("Aikaterine");
        stage.show();
    }

    public static void main(String[] args) {
        launch();
    }

}
