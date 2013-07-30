package gis

import (
	"bufio"
	"code.google.com/p/draw2d/draw2d"
	"fmt"
	"github.com/ajstarks/svgo"
	"github.com/pmylund/go-cache"
	"image"
	"image/color"
	"image/draw"
	"image/png"
	"io"
	"math"
	"os"
	"time"
)

type Mapper interface {
	DrawVector(*VectorShape, color.NRGBA, color.NRGBA, float64)
	Save()
}

type RasterMap struct {
	N, S, E, W    float64 // geographic boundaries of map
	width, height int     // pixel dimensions of map
	FileName      string
	I             draw.Image
	GC            draw2d.GraphicContext
}

func NewRasterMap(N, S, E, W float64, width int, filename string) *RasterMap {
	r := new(RasterMap)
	r.N, r.S, r.E, r.W = N, S, E, W
	r.FileName = filename
	r.width, r.height = width, int(float64(width)*(N-S)/(E-W))
	r.I = image.NewRGBA(image.Rect(0, 0, r.width, r.height))
	r.GC = draw2d.NewGraphicContext(r.I)
	return r
}

func (r *RasterMap) DrawVector(v *VectorShape, strokeColor,
	fillColor color.NRGBA, linewidth float64) {
	// check bounding box
	if v.Xmax < r.W || v.Xmin > r.E || v.Ymax < r.S || v.Ymin > r.N {
		return
	}
	x, y := v.calculateGridCoordinatesFloat(r.N, r.S, r.E, r.W, r.width, r.height)
	r.GC.SetStrokeColor(strokeColor)
	r.GC.SetFillColor(fillColor)
	r.GC.MoveTo(x[0], y[0])
	for i := 1; i < len(x); i++ {
		r.GC.LineTo(x[i], y[i])
	}
	r.GC.FillStroke()
}

func (r *RasterMap) Save() {
	f, err := os.Create(r.FileName)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	b := bufio.NewWriter(f)
	err = png.Encode(b, r.I)
	if err != nil {
		panic(err)
	}
	err = b.Flush()
	if err != nil {
		panic(err)
	}
}

type SVGmap struct {
	N, S, E, W    float64 // geographic boundaries of map
	width, height int     // pixel dimensions of map
	FileName      string
	Canvas        *svg.SVG
	f             *os.File
}

func NewSVGmap(N, S, E, W float64, width int, filename string) *SVGmap {
	var err error
	s := new(SVGmap)
	s.N, s.S, s.E, s.W = N, S, E, W
	s.FileName = filename
	s.width, s.height = width, int(float64(width)*(N-S)/(E-W))
	s.f, err = os.Create(filename)
	if err != nil {
		panic(err)
	}
	s.Canvas = svg.New(s.f)
	s.Canvas.Start(s.width, s.height)
	return s
}

func (s *SVGmap) DrawVector(v *VectorShape, strokeColor,
	fillColor color.NRGBA, linewidth float64) {
	x, y := v.calculateGridCoordinatesInt(s.N, s.S, s.E, s.W, s.width, s.height)
	style := fmt.Sprintf("fill:rgb(%v,%v,%v);stroke:rgb(%v,%v,%v);"+
		"stroke-width: %v",
		fillColor.R, fillColor.G, fillColor.B,
		strokeColor.R, strokeColor.G, strokeColor.B, linewidth)
	switch v.Shapetype {
	case "Point":
		s.Canvas.Circle(x[0], y[0], 1, style)
	case "LineString":
		s.Canvas.Polyline(x, y, style)
	case "Polygon", "MultiPolygon":
		s.Canvas.Polygon(x, y, style)
	default:
		panic(fmt.Sprintf("Type %v not supported.", v.Shapetype))
	}
}

func (s *SVGmap) Save() {
	s.Canvas.End()
	s.f.Close()
}

func (v *VectorShape) calculateGridCoordinatesInt(N, S, E, W float64,
	nx, ny int) (x []int, y []int) {
	x = make([]int, len(v.X))
	y = make([]int, len(v.Y))
	dx := (E - W) / float64(nx)
	dy := (N - S) / float64(ny)
	for i := 0; i < len(v.X); i++ {
		x[i] = int((v.X[i] - W) / dx)
		y[i] = ny - 1 - int((v.Y[i]-S)/dy)
	}
	return
}

func (v *VectorShape) calculateGridCoordinatesFloat(N, S, E, W float64,
	nx, ny int) (x []float64, y []float64) {
	x = make([]float64, len(v.X))
	y = make([]float64, len(v.Y))
	dx := (E - W) / float64(nx)
	dy := (N - S) / float64(ny)
	for i := 0; i < len(v.X); i++ {
		x[i] = (v.X[i] - W) / dx
		y[i] = float64(ny) - 1. - (v.Y[i]-S)/dy
	}
	return
}

type MapData struct {
	Cmap      *ColorMap
	Shapes    []*VectorShape
	Data      []float64
	tileCache *cache.Cache
}

func NewMapData(numShapes int, colorScheme string) *MapData {
	m := new(MapData)
	m.Cmap = NewColorMap(colorScheme)
	m.Shapes = make([]*VectorShape, numShapes)
	m.Data = make([]float64, numShapes)
	m.tileCache = cache.New(1*time.Hour, 10*time.Minute)
	return m
}

func (m *MapData) WriteGoogleMapTile(w io.Writer, zoom, x, y int64) {
	// Check if image is already in the cache.
	cacheKey := fmt.Sprintf("%v_%v_%v", zoom, x, y)
	if img, found := m.tileCache.Get(cacheKey); found {
		// send copy so original is not altered.
		err := png.Encode(w, img.(image.Image))
		if err != nil {
			panic(err)
		}
		return
	}
	strokeColor := color.NRGBA{0, 0, 0, 0}
	N, S, E, W := getGoogleTileBounds(zoom, x, y)
	maptile := NewRasterMap(N, S, E, W, 256, "")
	for i, shp := range m.Shapes {
		fillColor := m.Cmap.GetColor(m.Data[i])
		maptile.DrawVector(shp, strokeColor, fillColor, 0)
	}
	err := png.Encode(w, maptile.I)
	if err != nil {
		panic(err)
	}
	m.tileCache.Set(cacheKey, maptile.I, 0)
}

const originShift = math.Pi * 6378137. // for mercator projection

func getGoogleTileBounds(zoom, x, y int64) (N, S, E, W float64) {
	// get boundaries in lat/lon
	n := math.Pow(2, float64(zoom))
	W_lon := float64(x)/n*360.0 - 180.0
	E_lon := float64(x+1)/n*360.0 - 180.0
	N_rad := math.Atan(math.Sinh(math.Pi * (1 - 2*float64(y)/n)))
	N_lat := N_rad * 180.0 / math.Pi
	S_rad := math.Atan(math.Sinh(math.Pi * (1 - 2*float64(y+1)/n)))
	S_lat := S_rad * 180.0 / math.Pi
	// convert to Mercator meters
	W = W_lon * originShift / 180.0
	E = E_lon * originShift / 180.0
	N = math.Log(math.Tan((90+N_lat)*math.Pi/360.0)) /
		(math.Pi / 180.0) * originShift / 180.0
	S = math.Log(math.Tan((90+S_lat)*math.Pi/360.0)) /
		(math.Pi / 180.0) * originShift / 180.0
	return
}
