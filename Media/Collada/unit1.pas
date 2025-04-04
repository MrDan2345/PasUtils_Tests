unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CommonUtils, MediaUtils, Setup, PasOpenGL;

type TCamera = record
  P: TUMat;
  V: TUMat;
end;

type TForm1 = class(TCommonForm)
public
  var Scene: TUSceneData;
  var StartTime: UInt64;
  var CurTime: UInt64;
  var Camera: TCamera;
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
  procedure DrawAxis;
  procedure DrawMesh;
end;

type TUScopedTimer = record
strict private
  var _Start: UInt64;
  procedure Initialize;
  procedure Finalize;
public
  class operator Initialize(var v: TUScopedTimer);
  class operator Finalize(var v: TUScopedTimer);
end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Initialize;
  var ScopedTimer: TUScopedTimer;
begin
  StartTime := GetTickCount64;
  Scene := TUSceneDataDAE.Create([sdo_optimize, sdo_gen_normals, sdo_gen_tangents], sdu_z);
  //Scene.Load('Assets/X_Bot.dae');
  //Scene.Load('Assets/boxes3.dae');
  //Scene.Load('Assets/skin.dae');
  Scene.Load('Assets/box.dae');
  //Scene.Load('Assets/plane.dae');
  WriteLn(Length(Scene.MeshList));
end;

procedure TForm1.Finalize;
begin
  Scene.Free;
end;

procedure TForm1.Tick;
begin
  CurTime := GetTickCount64 - StartTime;
  Camera.V := TUMat.View(TUVec3.Make(10, 10, 10), TUVec3.Make(0, 0, 0), TUVec3.Make(0, 0, 1));
  Camera.P := TUMat.Proj(Pi / 4, ClientWidth / ClientHeight, 1, 100);
  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0.2, 0.2, 0.2, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  DrawAxis;
  DrawMesh;
end;

procedure TForm1.DrawAxis;
  var WV: TUMat;
  const AxisSize = 1000;
begin
  WV := Camera.V;
  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(@WV);
  glBegin(GL_LINES);
    glColor3f(1, 0, 0);
    glVertex3f(-AxisSize, 0, 0);
    glVertex3f(AxisSize, 0, 0);
    glColor3f(0, 1, 0);
    glVertex3f(0, -AxisSize, 0);
    glVertex3f(0, AxisSize, 0);
    glColor3f(0, 0, 1);
    glVertex3f(0, 0, -AxisSize);
    glVertex3f(0, 0, AxisSize);
    glColor3f(1, 1, 1);
  glEnd;
end;

procedure TForm1.DrawMesh;
  procedure SetupTransforms(const Xf: TUMat);
    var W, V, P, WV: TUMat;
  begin
    W := Xf * TUMat.RotationZ(((CurTime mod 10000) / 10000) * UTwoPi);
    V := Camera.V;
    P := Camera.P;
    WV := W * V;
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@WV);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@P);
  end;
  procedure RenderNode(const Node: TUSceneData.TNodeInterface);
    procedure RenderMesh(const MeshAttach: TUSceneData.TAttachmentMesh);
      procedure RenderSubset(Subset: TUSceneData.TMeshInterface.TSubset);
        function VertexData(const Vertex, Offset: Int32): Pointer;
        begin
          Result := Subset.VertexData + (Subset.Index[Vertex] * Subset.VertexSize) + Offset;
        end;
        var vd: TUVertexDescriptor;
        var i, d: Int32;
        var PosId, NormalId, TangentId, BinormalId: Int32;
        var PosOff, NormalOff, TangentOff, BinormalOff: Int32;
        var v, n: TUVec3;
      begin
        vd := Subset.VertexDescriptor;
        PosId := -1;
        NormalId := -1;
        TangentId := -1;
        BinormalId := -1;
        for d := 0 to High(vd) do
        begin
          case vd[d].Semantic of
            as_position:
            begin
              PosId := d;
              PosOff := UComputeVertexAttributeOffset(vd, d);
            end;
            as_normal:
            begin
              NormalId := d;
              NormalOff := UComputeVertexAttributeOffset(vd, d);
            end;
            as_tangent:
            begin
              TangentId := d;
              TangentOff := UComputeVertexAttributeOffset(vd, d);
            end;
            as_binormal:
            begin
              BinormalId := d;
              BinormalOff := UComputeVertexAttributeOffset(vd, d);
            end
            else;
          end;
        end;
        if PosId = -1 then Exit;
        for d := 0 to High(vd) do
        begin
          case vd[d].Semantic of
            as_position:
            begin
              glBegin(GL_TRIANGLES);
              glColor3f(1, 1, 1);
              for i := 0 to Subset.IndexCount - 1 do
              begin
                v := PUVec3(VertexData(i, PosOff))^;
                glVertex3fv(@v);
              end;
              glEnd();
            end;
            as_normal:
            begin
              //Continue;
              glBegin(GL_LINES);
              glColor3f(0, 0, 1);
              for i := 0 to Subset.IndexCount - 1 do
              begin
                v := PUVec3(VertexData(i, PosOff))^;
                n := PUVec3(VertexData(i, NormalOff))^;
                glVertex3fv(@v);
                v := v + n;
                glVertex3fv(@v);
              end;
              glEnd();
            end;
            as_tangent:
            begin
              //Continue;
              glBegin(GL_LINES);
              glColor3f(1, 0, 0);
              for i := 0 to Subset.IndexCount - 1 do
              begin
                v := PUVec3(VertexData(i, PosOff))^;
                n := PUVec3(VertexData(i, TangentOff))^;
                glVertex3fv(@v);
                v := v + n;
                glVertex3fv(@v);
              end;
              glEnd();
            end;
            as_binormal:
            begin
              //Continue;
              glBegin(GL_LINES);
              glColor3f(0, 1, 0);
              for i := 0 to Subset.IndexCount - 1 do
              begin
                v := PUVec3(VertexData(i, PosOff))^;
                n := PUVec3(VertexData(i, BinormalOff))^;
                glVertex3fv(@v);
                v := v + n;
                glVertex3fv(@v);
              end;
              glEnd();
            end;
            else;
          end;
        end;
        glColor3f(1, 1, 1);
      end;
      var i: Int32;
    begin
      for i := 0 to High(MeshAttach.Mesh.Subsets) do
      begin
        RenderSubset(MeshAttach.Mesh.Subsets[i]);
      end;
    end;
    procedure RenderSkin(const SkinAttach: TUSceneData.TAttachmentSkin);
      procedure RenderSubset(
        MeshSubset: TUSceneData.TMeshInterface.TSubset;
        SkinSubset: TUSceneData.TSkinInterface.TSubset
      );
        var i, w: Int32;
        var S: TUMat;
        var WeightsOffset: UInt32;
        var Vertex: PUVec3;
        var JointIndices: PUInt32Arr;
        var JointWeights: PUFloatArr;
        var VertexSkin: TUVec3;
        var SkinJoint: TUSceneData.TNodeInterface;
        var SkXf, SkBind, ShapeBind: TUMat;
      begin
        WeightsOffset := SkinSubset.WeightCount * SizeOf(UInt32);
        for i := 0 to MeshSubset.IndexCount - 1 do
        begin
          Vertex := PUVec3(MeshSubset.VertexData + (MeshSubset.Index[i] * MeshSubset.VertexSize));
          JointIndices := PUInt32Arr(SkinSubset.VertexData + (MeshSubset.Index[i] * SkinSubset.VertexSize));
          JointWeights := PUFloatArr(Pointer(JointIndices) + WeightsOffset);
          VertexSkin := TUVec3.Zero;
          ShapeBind := SkinAttach.Skin.ShapeBind;
          for w := 0 to SkinSubset.WeightCount - 1 do
          begin
            SkinJoint := SkinAttach.JointBindings[JointIndices^[w]];
            SkXf := SkinJoint.Transform;
            SkBind := SkinAttach.Skin.Joints[JointIndices^[w]].Bind;
            S := ShapeBind * SkBind * SkXf;
            VertexSkin += Vertex^.Transform4x3(S) * JointWeights^[w];
          end;
          glVertex3fv(@VertexSkin);
        end;
      end;
      var i: Int32;
    begin
      glBegin(GL_TRIANGLES);
      for i := 0 to High(SkinAttach.Skin.Mesh.Subsets) do
      begin
        RenderSubset(SkinAttach.Skin.Mesh.Subsets[i], SkinAttach.Skin.Subsets[i]);
      end;
      glEnd();
    end;
    var i: Int32;
    var Xf: TUMat;
  begin
    Xf := Node.Transform;
    SetupTransforms(Xf);
    //SetupTransforms(TUMat.Identity);
    for i := 0 to High(Node.Attachments) do
    begin
      if (Node.Attachments[i] is TUSceneData.TAttachmentMesh) then
      begin
        RenderMesh(TUSceneData.TAttachmentMesh(Node.Attachments[i]));
      end
      else if (Node.Attachments[i] is TUSceneData.TAttachmentSkin) then
      begin
        RenderSkin(TUSceneData.TAttachmentSkin(Node.Attachments[i]));
      end;
    end;
    for i := 0 to High(Node.Children) do
    begin
      RenderNode(Node.Children[i]);
    end;
  end;
  procedure UpdateAnimations(const Time: TUFloat);
    procedure UpdateTracks(const Animation: TUSceneData.TAnimationInterface);
      var i: Int32;
      var Xf: TUMat;
    begin
      for i := 0 to High(Animation.Tracks) do
      begin
        Xf := Animation.Tracks[i].Sample(Time, True);
        Animation.Tracks[i].Target.LocalTransform := Xf;
      end;
    end;
    var i: Int32;
  begin
    for i := 0 to High(Scene.AnimationList) do
    begin
      UpdateTracks(Scene.AnimationList[i]);
    end;
  end;
  procedure RenderMesh(const Mesh: TUSceneData.TMeshInterface);
    procedure RenderSubset(const Subset: TUSceneData.TMeshInterface.TSubset);
      var i: Int32;
      var v: TUVec3;
    begin
      glBegin(GL_TRIANGLES);
      for i := 0 to Subset.IndexCount - 1 do
      begin
        glColor3f(0.0, 1.0, 0.0);
        v := PUVec3(Subset.VertexData + Subset.Index[i] * Subset.VertexSize)^;
        glVertex3fv(@v);
      end;
      glEnd();
      glColor3f(1.0, 1.0, 1.0);
    end;
    var i: Int32;
  begin
    SetupTransforms(TUMat.Identity);
    for i := 0 to High(Mesh.Subsets) do
    begin
      RenderSubset(Mesh.Subsets[i]);
    end;
  end;
begin
  UpdateAnimations(TUFloat(CurTime) * 0.0001);
  //glEnable(GL_TEXTURE_2D);
  glShadeModel(GL_FLAT);
  //glShadeModel(GL_SMOOTH);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glDisable(GL_CULL_FACE);
  //glEnable(GL_BLEND);
  RenderNode(Scene.RootNode);
  if Length(Scene.MeshList) > 0 then
  begin
    //RenderMesh(Scene.MeshList[(CurTime div 5000) mod Length(Scene.MeshList)]);
  end;
end;

procedure TUScopedTimer.Initialize;
begin
  _Start := GetTickCount64;
end;

procedure TUScopedTimer.Finalize;
begin
  WriteLn('Scoped Timer: ', GetTickCount64 - _Start);
end;

class operator TUScopedTimer.Initialize(var v: TUScopedTimer);
begin
  v.Initialize;
end;

class operator TUScopedTimer.Finalize(var v: TUScopedTimer);
begin
  v.Finalize;
end;

end.

