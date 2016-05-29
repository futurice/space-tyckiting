"use strict";

var _ = require("lodash");
var tools = require("./rules/tools");

function combineOtherTeams(player, players, bots) {
    return _.chain(players)
        .filter(function (other) {
            return other.teamId !== player.teamId;
        })
        .map(function (other) {
            return {
                name: other.name,
                teamId: other.teamId,
                bots: _.filter(bots, function (bot) {
                    return bot.teamId !== player.teamId;
                }).map(tools.botInfo)
            };
        })
        .value();
}

function combineYourTeam(player, bots) {
    return {
        name: player.name,
        teamId: player.teamId,
        bots: _.where(bots, { teamId: player.teamId }).map(tools.botInfoWithPositionAndHp)
    };
}

function combineTeams(players, bots) {
    return players.map(function (player) {
        return combineYourTeam(player, bots);
    });
}

function mkUserConfig(config) {
    return config;
}

function mkStartMessage(player, players, bots, config) {
    return {
        type: "start",
        you: combineYourTeam(player, bots),
        config: mkUserConfig(config),
        otherTeams: combineOtherTeams(player, players, bots)
    };
}

function mkRoundSummaryMessage(players, bots, config, asteroids, roundId, actions, events) {
    console.log("EVENTS", events);
    return {
        type: "round",
        roundId: roundId,
        config: config,
        teams: combineTeams(players, bots),
        asteroids: asteroids,
        events: events,
        actions: actions
    };
}

function mkEventsMessage(player, players, bots, config, roundId, events) {
    return {
        type: "events",
        roundId: roundId,
        config: mkUserConfig(config),
        you: combineYourTeam(player, bots),
        otherTeams: combineOtherTeams(player, players, bots),
        events: _.shuffle(events)
    };
}

function mkEndMessage(player, bots, winnerTeamId) {
    return {
        type: "end",
        you: combineYourTeam(player, bots),
        winnerTeamId: winnerTeamId
    };
}

function mkEndSummaryMessage(players, bots, winnerTeamId) {
    return {
        type: "endSummary",
        teams: combineTeams(players, bots),
        winnerTeamId: winnerTeamId
    };
}

function spectatorStartMessage(players, bots, config) {
    return {
        type: "start",
        teams: combineTeams(players, bots),
        config: config
    };
}

module.exports = {
    combineOtherTeams: combineOtherTeams,
    combineYourTeam: combineYourTeam,
    mkUserConfig: mkUserConfig,
    mkStartMessage: mkStartMessage,
    mkEventsMessage: mkEventsMessage,
    mkEndMessage: mkEndMessage,
    mkEndSummaryMessage: mkEndSummaryMessage,
    mkRoundSummaryMessage: mkRoundSummaryMessage,
    spectatorStartMessage: spectatorStartMessage
};
