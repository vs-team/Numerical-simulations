using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Windows.Forms.DataVisualization.Charting;

namespace EntryPoint
{
  struct ApplicationStats
  {
    public int Dead;
    public int Susceptible;
    public int Infected;
    public int Immune;
  }

  public class SimulationEntryPoint : Game
  {
    GraphicsDeviceManager graphics;
    SpriteBatch spriteBatch;
    Texture2D man, woman;
    Texture2D city;
    Texture2D chart;
    SpriteFont uiFont;
    ContentManager chartsContent;
    bool alwaysShowChart = true;

    List<ApplicationStats> stats = new List<ApplicationStats>();

    SimulationState.SimState simState;

    public SimulationEntryPoint()
    {
      graphics = new GraphicsDeviceManager(this);
      graphics.PreferredBackBufferWidth = 1280;
      graphics.PreferredBackBufferHeight = 800;
      graphics.IsFullScreen = true;
      Content.RootDirectory = "Content";
    }

    protected override void Initialize()
    {
      var cityPositions =
        from i in Enumerable.Range(0, 3)
        from j in Enumerable.Range(0, 2)
        select new Vector2(200 + i * 440, 150 + j * 400);
      simState = Simulation.setupState(
        cityPositions, 1000,
        75, 24, 7,
        0.01, 4);

      base.Initialize();
    }

    protected override void LoadContent()
    {
      chartsContent = new ContentManager(this.Services);
      chartsContent.RootDirectory = "Content";

      spriteBatch = new SpriteBatch(GraphicsDevice);

      man = Content.Load<Texture2D>("man_face");
      woman = Content.Load<Texture2D>("woman_face");
      city = Content.Load<Texture2D>("city");
      uiFont = Content.Load<SpriteFont>("UIFont");
      chart = Content.Load<Texture2D>("whitePixel");
    }

    KeyboardState prevKS = new KeyboardState();
    Stopwatch sw = Stopwatch.StartNew();
    Stopwatch chart_sw = Stopwatch.StartNew();
    bool autoRun = false;
    protected override void Update(GameTime gameTime)
    {
      var currKS = Keyboard.GetState();
      if (currKS.IsKeyDown(Keys.Escape))
        Exit();

      if ((autoRun && sw.ElapsedMilliseconds > 100) || (currKS.IsKeyDown(Keys.Space) && (prevKS.IsKeyUp(Keys.Space) || sw.ElapsedMilliseconds > 500)))
      {
        int dead = 0, immune = 0, infected = 0, susceptible = 0;
        foreach (var a in simState.Agents)
        {
          if (a.Value.Infection.IsDead) dead++;
          else if (a.Value.Infection.IsImmune) immune++;
          else if (a.Value.Infection.IsInfected) infected++;
          else if (a.Value.Infection.IsSusceptible) susceptible++;
        }
        stats.Add(
          new ApplicationStats()
          {
            Dead = dead,
            Immune = immune,
            Infected = infected,
            Susceptible = susceptible
          });
        simState = Simulation.updateState(simState);
        sw.Restart();
      }

      if (currKS.IsKeyDown(Keys.Enter) && prevKS.IsKeyUp(Keys.Enter))
        autoRun = !autoRun;

      if ((alwaysShowChart && chart_sw.ElapsedMilliseconds > 200) || currKS.IsKeyDown(Keys.Tab) && (prevKS.IsKeyUp(Keys.Tab) || chart_sw.ElapsedMilliseconds > 200))
      {

        try { GenerateInfectionChart(); } catch (Exception) { }
        chart_sw.Restart();
      }

      prevKS = currKS;
      base.Update(gameTime);
    }

    void GenerateInfectionChart()
    {
      // create the chart
      var chartControl = new Chart();
      chartControl.Size = new System.Drawing.Size(500, 500);

      var chartArea = new ChartArea();
      chartArea.AxisX.MajorGrid.LineColor = System.Drawing.Color.LightGray;
      chartArea.AxisY.MajorGrid.LineColor = System.Drawing.Color.LightGray;
      chartArea.AxisX.LabelStyle.Font = new System.Drawing.Font("Consolas", 8);
      chartArea.AxisY.LabelStyle.Font = new System.Drawing.Font("Consolas", 8);
      chartControl.ChartAreas.Add(chartArea);
      chartControl.Legends.Add("Legend");
      chartControl.Legends["Legend"].LegendStyle = LegendStyle.Table;
      chartControl.Legends["Legend"].Docking = Docking.Bottom;
      chartControl.Legends["Legend"].IsDockedInsideChartArea = true;


      var series = new Series();
      series.Name = "Susceptible";
      series.ChartType = SeriesChartType.FastLine;
      series.XValueType = ChartValueType.Auto;
      series.Color = System.Drawing.Color.Pink;
      series.Points.DataBindY((from s in stats select s.Susceptible).ToArray());
      chartControl.Series.Add(series);

      series = new Series();
      series.Name = "Immune";
      series.ChartType = SeriesChartType.FastLine;
      series.XValueType = ChartValueType.Auto;
      series.Color = System.Drawing.Color.CornflowerBlue;
      series.Points.DataBindY((from s in stats select s.Immune).ToArray());
      chartControl.Series.Add(series);

      series = new Series();
      series.Name = "Infected";
      series.ChartType = SeriesChartType.FastLine;
      series.XValueType = ChartValueType.Auto;
      series.Color = System.Drawing.Color.YellowGreen;
      series.Points.DataBindY((from s in stats select s.Infected).ToArray());
      chartControl.Series.Add(series);

      // draw!
      chartControl.Invalidate();

      // write out a file
      chartControl.SaveImage(@"Content\chart.png", ChartImageFormat.Png);
      chartsContent.Unload();
      chart = chartsContent.Load<Texture2D>("chart.png");
    }

    protected override void Draw(GameTime gameTime)
    {
      GraphicsDevice.Clear(Color.Gray);

      spriteBatch.Begin();

      foreach (var x in simState.Cities)
        spriteBatch.DrawCentered(city, x.Position, Color.White);
      foreach (var x in simState.Agents)
      {
        var face = x.Value.Gender.IsMale ? man : woman;
        var color = Color.White;
        if (x.Value.Infection.IsInfected)
          color = Color.YellowGreen;
        else if (x.Value.Infection.IsImmune)
          color = Color.CornflowerBlue;
        else if (x.Value.Infection.IsDead)
          color = Color.DarkGray;
        spriteBatch.DrawCentered(face, x.Value.City.Position + x.Value.OffsetPosition, color);
      }

      var time = DateTime.MinValue + TimeSpan.FromHours(simState.LastUpdateTime);
      for (int i = -1; i < 2; i++)
        for (int j = -1; j < 2; j++)
          spriteBatch.DrawString(uiFont, time.ToString("hh - dd/MM/yy"), new Vector2(20 + i, 20 + j), Color.Black);
      spriteBatch.DrawString(uiFont, time.ToString("hh - dd/MM/yy"), new Vector2(20, 20), Color.White);

      if (alwaysShowChart || Keyboard.GetState().IsKeyDown(Keys.Tab))
        spriteBatch.Draw(chart, new Vector2(0, 0), Color.White);

      spriteBatch.End();

      base.Draw(gameTime);
    }
  }

  static class Extensions
  {
    public static void DrawCentered(this SpriteBatch spriteBatch, Texture2D t, Vector2 p, Color c)
    {
      spriteBatch.Draw(t, p - new Vector2(t.Width, t.Height) / 2, c);
    }
  }
}
