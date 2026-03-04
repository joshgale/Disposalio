"use client";

import React, { useState, useMemo, useEffect } from "react";
import { 
  Search, 
  ChevronDown, 
  TrendingUp, 
  TrendingDown, 
  Minus,
  Filter
} from "lucide-react";
import { clsx, type ClassValue } from "clsx";
import { twMerge } from "tailwind-merge";

function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

type Player = {
  name: string;
  team: string;
  opponent: string;
  disposals: number[];
  trend: number;
};

const DISPOSAL_BADGE_COLORS = (val: number) => {
  if (val > 25) return "bg-emerald-100 text-emerald-700";
  if (val >= 15) return "bg-amber-100 text-amber-700";
  return "bg-rose-100 text-rose-700";
};

export default function DisposalIO() {
  const [players, setPlayers] = useState<Player[]>([]);
  const [search, setSearch] = useState("");
  const [matchFilter, setMatchFilter] = useState("All Games");
  const [historyCount, setHistoryCount] = useState<5 | 10 | 20>(10);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetch("/data/players.json")
      .then((res) => res.json())
      .then((data) => {
        setPlayers(data);
        setLoading(false);
      })
      .catch((err) => {
        console.error("Failed to load player data:", err);
        setLoading(false);
      });
  }, []);

  const uniqueMatches = useMemo(() => {
    const matches = new Set<string>();
    players.forEach((p) => {
      if (p.opponent && p.opponent !== "TBC") {
        const team1 = p.team;
        const team2 = p.opponent;
        const match = [team1, team2].sort().join(" vs ");
        matches.add(match);
      }
    });
    return Array.from(matches).sort();
  }, [players]);

  const filteredPlayers = useMemo(() => {
    return players.filter((player) => {
      const matchesSearch = player.name.toLowerCase().includes(search.toLowerCase());
      
      let matchesMatch = true;
      if (matchFilter !== "All Games") {
        const [t1, t2] = matchFilter.split(" vs ");
        matchesMatch = player.team === t1 || player.team === t2;
      }

      return matchesSearch && matchesMatch;
    });
  }, [players, search, matchFilter]);

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-50 text-slate-900">
        <div className="animate-pulse font-medium">Loading Disposal.io...</div>
      </div>
    );
  }

  return (
    <main className="min-h-screen bg-gray-50 text-slate-900 py-12 px-4 md:px-8">
      <div className="max-w-[1200px] mx-auto space-y-8">
        
        {/* Branding & Header */}
        <header className="flex flex-col md:flex-row md:items-end justify-between gap-6">
          <div className="space-y-1">
            <h1 className="text-4xl font-bold tracking-tight text-slate-900">Disposal.io</h1>
            <p className="text-slate-500 font-medium">AFL Player Disposal Tracker (2026 Season)</p>
          </div>

          <div className="relative w-full md:w-96">
            <Search className="absolute left-4 top-1/2 -translate-y-1/2 h-4 w-4 text-slate-400" />
            <input 
              type="text" 
              placeholder="Search player name..."
              className="w-full pl-11 pr-4 py-3 bg-white border border-slate-200 rounded-2xl shadow-sm focus:outline-hidden focus:ring-2 focus:ring-blue-600/20 transition-all"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
            />
          </div>
        </header>

        {/* Filters */}
        <div className="flex flex-col md:flex-row gap-4 items-center justify-between">
          <div className="flex items-center gap-3 w-full md:w-auto">
            <div className="relative w-full md:w-64">
              <Filter className="absolute left-4 top-1/2 -translate-y-1/2 h-4 w-4 text-slate-400" />
              <select 
                className="w-full pl-11 pr-10 py-2.5 bg-white border border-slate-200 rounded-2xl shadow-sm appearance-none focus:outline-hidden focus:ring-2 focus:ring-blue-600/20 transition-all cursor-pointer"
                value={matchFilter}
                onChange={(e) => setMatchFilter(e.target.value)}
              >
                <option>All Games</option>
                {uniqueMatches.map((m) => (
                  <option key={m} value={m}>{m}</option>
                ))}
              </select>
              <ChevronDown className="absolute right-4 top-1/2 -translate-y-1/2 h-4 w-4 text-slate-400 pointer-events-none" />
            </div>
          </div>

          <div className="inline-flex bg-slate-100 p-1 rounded-2xl shadow-inner w-full md:w-auto">
            {[5, 10, 20].map((num) => (
              <button
                key={num}
                onClick={() => setHistoryCount(num as any)}
                className={cn(
                  "px-6 py-2 rounded-xl text-sm font-semibold transition-all",
                  historyCount === num 
                    ? "bg-white text-slate-900 shadow-sm" 
                    : "text-slate-500 hover:text-slate-700"
                )}
              >
                Last {num}
              </button>
            ))}
          </div>
        </div>

        {/* Table Card */}
        <div className="bg-white rounded-2xl shadow-sm border border-slate-100 overflow-hidden">
          <div className="overflow-x-auto">
            <table className="w-full text-left border-collapse">
              <thead>
                <tr className="bg-slate-50 border-b border-slate-100">
                  <th className="px-6 py-4 font-semibold text-slate-600 text-sm uppercase tracking-wider">Player</th>
                  <th className="px-6 py-4 font-semibold text-slate-600 text-sm uppercase tracking-wider">Team</th>
                  <th className="px-6 py-4 font-semibold text-slate-600 text-sm uppercase tracking-wider">Opponent</th>
                  <th className="px-6 py-4 font-semibold text-slate-600 text-sm uppercase tracking-wider">Recent Disposals</th>
                  <th className="px-6 py-4 font-semibold text-slate-600 text-sm uppercase tracking-wider">Avg</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-slate-50">
                {filteredPlayers.length === 0 ? (
                  <tr>
                    <td colSpan={5} className="px-6 py-12 text-center text-slate-500 font-medium">
                      No players found matching your filters.
                    </td>
                  </tr>
                ) : (
                  filteredPlayers.map((player, idx) => {
                    const recentStats = player.disposals.slice(0, historyCount);
                    const avg = (recentStats.reduce((a, b) => a + b, 0) / recentStats.length).toFixed(1);

                    return (
                      <tr key={idx} className="hover:bg-slate-50/50 transition-colors">
                        <td className="px-6 py-5 font-bold text-slate-900">{player.name}</td>
                        <td className="px-6 py-5 text-slate-600 text-sm font-medium">{player.team}</td>
                        <td className="px-6 py-5">
                          <span className="bg-blue-50 text-blue-700 px-3 py-1 rounded-full text-xs font-bold uppercase tracking-tight">
                            {player.opponent}
                          </span>
                        </td>
                        <td className="px-6 py-5">
                          <div className="flex flex-wrap gap-1.5 max-w-64">
                            {recentStats.map((d, i) => (
                              <span 
                                key={i} 
                                className={cn(
                                  "w-7 h-7 flex items-center justify-center rounded-lg text-[10px] font-bold shadow-xs",
                                  DISPOSAL_BADGE_COLORS(d)
                                )}
                              >
                                {d}
                              </span>
                            ))}
                          </div>
                        </td>
                        <td className="px-6 py-5">
                          <div className="flex items-center gap-2">
                            <span className="text-lg font-extrabold tabular-nums">{avg}</span>
                            {player.trend === 1 && <TrendingUp className="h-4 w-4 text-emerald-500" />}
                            {player.trend === -1 && <TrendingDown className="h-4 w-4 text-rose-500" />}
                            {player.trend === 0 && <Minus className="h-4 w-4 text-slate-300" />}
                          </div>
                        </td>
                      </tr>
                    );
                  })
                )}
              </tbody>
            </table>
          </div>
        </div>

        {/* Footer info */}
        <footer className="pt-8 text-center text-slate-400 text-sm font-medium">
          Built for Disposal.io • Data refreshed daily via fitzRoy • {new Date().getFullYear()}
        </footer>

      </div>
    </main>
  );
}
