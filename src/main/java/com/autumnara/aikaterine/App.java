package com.autumnara.aikaterine;

public final class App {

    public static void main(String[] args)
    {
        VirtueGraph graph = new VirtueGraph();

        Virtue virtueLearning  = new MutableVirtue("Learning",  "Knowledge about the arts and the sciences.");
        Virtue virtuePhysical  = new MutableVirtue("Physical",  "Physical strength, endurance, and mobility.");
        Virtue virtueEmotional = new MutableVirtue("Emotional", "Emotional stability and intuition.");
        Virtue virtueArts      = new MutableVirtue("Arts",      "Knowledge and skill in literature, history, and art.");
        Virtue virtueSciences  = new MutableVirtue("Sciences",  "Knowledge and skill in science and mathematics.");
        Virtue virtueStrength  = new MutableVirtue("Strength",  "The ability to push or pull with force.");
        Virtue virtueEndurance = new MutableVirtue("Endurance", "Sustaining physical activity for a long time.");
        Virtue virtueMobility  = new MutableVirtue("Mobility",  "The ability to move the body in various ways.");
        Virtue virtueStability = new MutableVirtue("Stability", "When you maintain composure in difficult times.");
        Virtue virtueIntuition = new MutableVirtue("Intuition", "The ability to read emotions.");

        graph.addVertex(virtueLearning);
        graph.addVertex(virtuePhysical);
        graph.addVertex(virtueEmotional);
        graph.addVertex(virtueArts);
        graph.addVertex(virtueSciences);
        graph.addVertex(virtueStrength);
        graph.addVertex(virtueEndurance);
        graph.addVertex(virtueMobility);
        graph.addVertex(virtueStability);
        graph.addVertex(virtueIntuition);

        graph.addEdge(graph.root, virtueLearning);
        graph.addEdge(graph.root, virtuePhysical);
        graph.addEdge(graph.root, virtueEmotional);
        graph.addEdge(virtueLearning, virtueArts);
        graph.addEdge(virtueLearning, virtueSciences);
        graph.addEdge(virtuePhysical, virtueStrength);
        graph.addEdge(virtuePhysical, virtueEndurance);
        graph.addEdge(virtuePhysical, virtueMobility);
        graph.addEdge(virtueEmotional, virtueStability);
        graph.addEdge(virtueEmotional, virtueIntuition);

        UIComponent root = new UIGraph(graph, new BoundedFloat(5, 10, 15));

        Window window = new Window(600, 400, "Aikaterine", root);

        window.initialize();
        window.loop();
        window.terminate();
    }

}
