import discord
import datetime
import secrets_bot
from discord.ext import commands 

# Needs manage thread permission
def bot_test():
    intents = discord.Intents.default()
    intents.message_content = True
    intents.members = True

    client = commands.Bot(command_prefix = ".", intents = intents)
    match_dict = dict()

    class MatchPrompt(discord.ui.View):
        def __init__(self, *, timeout = None):
            super().__init__(timeout = timeout)
        @discord.ui.button(label = "Start Match Submission", style = discord.ButtonStyle.blurple)
        async def blurple_button(self, interaction:discord.Interaction, button:discord.ui.Button):
            await interaction.response.defer()
            thread = await interaction.message.channel.create_thread(name="Match Submission")
            await thread.send("<@{}>\nWho played in this match?".format(interaction.user.id), view = PlayerSelection())

    class MatchConfirmation(discord.ui.View):
        def __init__(self, thread_id,*, timeout = 180):
            self.results_channel = secrets_bot.get_results_channel()
            self.thread_id = thread_id
            super().__init__(timeout = timeout)

        @discord.ui.button(label = "Cancel", style = discord.ButtonStyle.gray)
        #order of interaction and button need to be swapped from example. Maybe because of Nextcord vs. discord.py differences
        async def gray_button(self, interaction:discord.Interaction, button:discord.ui.Button):
            await interaction.response.defer()
            await interaction.followup.edit_message(message_id = interaction.message.id, view = None)
            await interaction.channel.edit(locked = True)
            await interaction.followup.send(content = "You have cancelled the match submission. This channel is now locked.")
            del match_dict[self.thread_id]

        @discord.ui.button(label = "Edit", style = discord.ButtonStyle.blurple)
        async def blurple_button(self, interaction:discord.Interaction, button:discord.ui.Button):
            await interaction.response.defer()
            await interaction.followup.edit_message(message_id = interaction.message.id, view = None)
            await interaction.followup.send(content = "<@{}>\nWho played in this match?".format(interaction.user.id), view = PlayerSelection())

        @discord.ui.button(label = "Submit", style = discord.ButtonStyle.green)
        async def green_button(self, interaction:discord.Interaction, button:discord.ui.Button):
            await interaction.response.defer()
            await interaction.followup.edit_message(message_id = interaction.message.id, view = None)
            results_channel = client.get_channel(self.results_channel)
            message = await results_channel.send(embed = match_dict[self.thread_id]["embed"]["tag"])
            await interaction.channel.edit(locked = True)
            await interaction.followup.send(content = "Thank you! Your submitted match can be seen [here]({}). This channel is now locked.".format(message.jump_url))
            del match_dict[self.thread_id]

    class MatchDetails(discord.ui.Modal, title = "Match Submission"):
        def __init__(self, thread_id, *args, **kwargs) -> None:
            self.thread_id = thread_id
            super().__init__(*args, **kwargs)

            for i in range(len(match_dict[thread_id]["Players"])):
                player = match_dict[thread_id]["Players"][i]["Name"]
                self.add_item(discord.ui.TextInput(label = player))
            self.add_item(discord.ui.TextInput(label = "Video Link", required = False))

        def build_embed(self):
            thread_id = self.thread_id
            match_dict[thread_id]["embed"] = dict()
            num_players = secrets_bot.get_num_players()
            player_lines_tags = [""] * num_players
            player_lines_names = [""] * num_players
            for i in range(len(match_dict[thread_id]["Players"])):
                player_data = match_dict[thread_id]["Players"][i]
                player_lines_tags[i] = "**<@{}>**: {}".format(player_data["Id"], player_data["Score"])
                player_lines_names[i] = "**{}**: {}".format(player_data["Name"], player_data["Score"])
            today_string = datetime.date.today().isoformat()
            match_summary_tags = "**Match Submission - <@{}>**".format(match_dict[thread_id]["Submitter"].id) + "\n\n" + "{}\n".format(today_string) + "\n".join(player_lines_tags) + "\n" + match_dict[thread_id]["Video"]
            match_summary_names = "**Match Submission - <@{}>**".format(match_dict[thread_id]["Submitter"].id) + "\n\n" + "{}\n".format(today_string) + "\n".join(player_lines_names) + "\n" + match_dict[thread_id]["Video"]
            embed_tag = discord.Embed(description = match_summary_tags, color = 0x00ff00)
            embed_name = discord.Embed(description = match_summary_names, color = 0x00ff00)
            match_dict[thread_id]["embed"]["tag"] = embed_tag
            match_dict[thread_id]["embed"]["name"] = embed_name

        async def on_submit(self, interaction:discord.Interaction):
            num_players = secrets_bot.get_num_players()
            thread_id = self.thread_id
            interaction_response = interaction.response
            submitted_values = [subcomponent["components"][0]["value"] for subcomponent in interaction.data["components"]]
            player_scores = submitted_values[:num_players]
            match_dict[thread_id]["Video"] = submitted_values[num_players]
            if not all([score.isdigit() for score in player_scores]):
                player_scores_not_numeric = ", ".join([score for score in player_scores if not score.isdigit()])
                await interaction_response.send_message('I could not understand the following score(s): `{}`. Please try again.\n\nWho played in this match?'.format(player_scores_not_numeric),view=PlayerSelection())
                return
            if sum([int(player_score) for player_score in player_scores]) != 96:
                await interaction_response.send_message('The listed scores (`{}`) do not add to 96 please try again. If there was a penalty, please contact a mod to submit the score. Otherwise, submit the match now.\n\nWho played in this match?'.format(', '.join(player_scores)),view=PlayerSelection())
                return
            for i in range(len(match_dict[thread_id]["Players"])):
                player = match_dict[thread_id]["Players"][i]["Name"]
                match_dict[thread_id]["Players"][i]["Score"] = player_scores[i]
            self.build_embed()
            await interaction_response.send_message(content = "Preview:", embed = match_dict[thread_id]["embed"]["name"], view = MatchConfirmation(thread_id=thread_id))

    class PlayerSelection(discord.ui.View):
        def __init__(self, *, timeout = 180):
            super().__init__(timeout = timeout)
        @discord.ui.select(cls = discord.ui.UserSelect, min_values = secrets_bot.get_num_players(), max_values = secrets_bot.get_num_players())
        async def select_callback(self, interaction:discord.Interaction, select): # the function called when the user is done selecting options
            thread_id = interaction.channel.id
            match_dict[thread_id] = dict()
            match_dict[thread_id]["Submitter"] = interaction.user
            match_dict[thread_id]["Players"] = dict()
            for i in range(len(select.values)):
                value = select.values[i]
                player = value.name
                match_dict[thread_id]["Players"][i] = dict()
                match_dict[thread_id]["Players"][i]["Name"] = value.name
                match_dict[thread_id]["Players"][i]["Id"] = value.id
            await interaction.response.send_modal(MatchDetails(thread_id = interaction.channel.id))

    @client.command()
    async def start(ctx):
        if ctx.channel.id == secrets_bot.get_submission_channel():
            await ctx.send("Click to enter match details", view = MatchPrompt())

    @client.event
    async def on_ready():
        print(f'We have logged in as {client.user}', flush = True)

    client.run(secrets_bot.get_bot_token())
if __name__ == "__main__":
    bot_test()