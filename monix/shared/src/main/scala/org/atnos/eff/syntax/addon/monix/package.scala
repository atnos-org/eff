package org.atnos.eff.syntax.addon

import org.atnos.eff.addon.monix.{AsyncTasks, TaskCreation, TaskInterpretation}

package object monix extends AsyncTasks with TaskInterpretation with TaskCreation
