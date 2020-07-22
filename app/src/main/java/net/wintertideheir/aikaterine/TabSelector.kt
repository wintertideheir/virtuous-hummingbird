package net.wintertideheir.aikaterine

import androidx.navigation.NavController
import com.google.android.material.tabs.TabLayout
import com.google.android.material.tabs.TabLayout.Tab

class TabSelector(val navController: NavController) : TabLayout.OnTabSelectedListener {

    override fun onTabReselected(p0: Tab) { }

    override fun onTabUnselected(p0: Tab) { }

    override fun onTabSelected(p0: Tab) {
        when (p0.contentDescription.toString()) {
            "tab1" -> navController.navigate(R.id.action_global_FirstFragment)
            "tab2" -> navController.navigate(R.id.action_global_SecondFragment)
            "tab3" -> navController.navigate(R.id.action_global_ThirdFragment)
        }
    }

}