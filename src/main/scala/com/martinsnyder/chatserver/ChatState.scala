package com.martinsnyder.chatserver

object ChatState {
  // Default constructor
  def apply(): ChatState = ChatState(Map.empty, Map.empty)
}

case class ChatState(
    userRooms: Map[String, String],
    roomMembers: Map[String, Set[String]],
    lastMessage: Map[String, Play] = Map.empty
) {

  def process(msg: InputMessage): (ChatState, Seq[OutputMessage]) = msg match {
    case Help(user) =>
      (this, Seq(SendToUser(user, InputMessage.HelpText)))

    case Chat(user, text) =>
      userRooms.get(user) match {
        case Some(room) =>
          (this, sendToRoom(room, s"$user: $text"))

        case None =>
          (this, Seq(SendToUser(user, "You are not currently in a room")))
      }

    case EnterRoom(user, toRoom) =>
          // Already in - move from one room to another
          val (intermediateState, _) = removeFromCurrentRoom(user)
          val (finalState, enterMessages)        = intermediateState.addToRoom(user, toRoom)

          (finalState,  lastMessage.get(toRoom).map(p => SendToUser(user, Play.toMessage(p))).toSeq ++ enterMessages)

    case ListRooms(user) =>
      val roomList = roomMembers.keys.toList.sorted
        .mkString("Rooms:\n\t", "\n\t", "")

      (this, Seq(SendToUser(user, roomList)))

    case ListMembers(user) =>
      val memberList = userRooms.get(user) match {
        case Some(room) =>
          roomMembers
            .getOrElse(room, Set())
            .toList
            .sorted
            .mkString("Room Members:\n\t", "\n\t", "")

        case None =>
          "You are not currently in a room"
      }

      (this, Seq(SendToUser(user, memberList)))

    case Disconnect(user) =>
      removeFromCurrentRoom(user)

    case InvalidInput(user, text) =>
      (this, Seq(SendToUser(user, s"Invalid input: $text")))

    case p @ Play(user, text) =>
      userRooms.get(user) match {
        case Some(room) =>
          (this.copy(lastMessage = lastMessage + ( room -> p)), sendToRoom(room, Play.toMessage(p)))

        case None =>
          (this, Seq(SendToUser(user, "You are not currently in a room")))
      }
  }

  private def sendToRoom(room: String, text: String): Seq[OutputMessage] = {
    roomMembers
      .get(room)
      .map(SendToUsers(_, text))
      .toSeq
  }

  private def removeFromCurrentRoom(user: String): (ChatState, Seq[OutputMessage]) = userRooms.get(user) match {
    case Some(room) =>
      val nextMembers = roomMembers.getOrElse(room, Set()) - user
      val nextState =
        if (nextMembers.isEmpty)
          this.copy(userRooms = userRooms - user, roomMembers - room)
        else
          this.copy(userRooms = userRooms - user, roomMembers = roomMembers + (room -> nextMembers))

      // Send to "previous" room population to include the leaving user
      (nextState, sendToRoom(room, s"$user has left $room"))
    case None =>
      (this, Nil)
  }

  private def addToRoom(user: String, room: String): (ChatState, Seq[OutputMessage]) = {
    val nextMembers = roomMembers.getOrElse(room, Set()) + user
    val nextState   = this.copy(userRooms = userRooms + (user -> room), roomMembers = roomMembers + (room -> nextMembers))

    // Send to "next" room population to include the joining user
    (nextState, nextState.sendToRoom(room, s"$user has joined $room"))
  }
}
