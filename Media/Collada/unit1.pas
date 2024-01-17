unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CommonUtils, MediaUtils, Setup, PasOpenGL;

type
  TForm1 = class(TCommonForm)
  public
    var Scene: TUSceneData;
    var StartTime: UInt64;
    var CurTime: UInt64;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Tick; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Initialize;
  procedure PropagateTransforms(const Node: TUSceneData.TNodeInterface);
    var i: Int32;
  begin
    if Assigned(Node.Parent) then
    begin
      Node.Transform := Node.Parent.Transform * Node.Transform;
    end;
    for i := 0 to High(Node.Children) do
    begin
      PropagateTransforms(Node.Children[i]);
    end;
  end;
begin
  Scene := TUSceneDataDAE.Create();
  Scene.Load('Assets/skin.dae');
  PropagateTransforms(Scene.RootNode);
  WriteLn(Length(Scene.MeshList));
end;

procedure TForm1.Finalize;
begin
  Scene.Free;
end;

procedure TForm1.Tick;
  procedure SetupTransforms(const Xf: TUMat);
    var W, V, P, WV: TUMat;
  begin
    W := Xf * TUMat.RotationZ(((GetTickCount64 mod 6000) / 6000) * 2 * Pi);
    V := TUMat.View(TUVec3.Make(0, 8, 0), TUVec3.Make(0, 0, 0), TUVec3.Make(0, 0, 1));
    P := TUMat.Proj(Pi / 4, 1, 1, 100);
    WV := W * V;
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@WV);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@P);
  end;
  procedure RenderNode(const Node: TUSceneData.TNodeInterface);
    procedure RenderMesh(const MeshAttach: TUSceneData.TAttachmentMesh);
      procedure RenderSubset(Subset: TUSceneData.TMeshInterface.TSubset);
        var i: Int32;
        var Vertex: PUVec3;
      begin
        for i := 0 to Subset.IndexCount - 1 do
        begin
          Vertex := PUVec3(Subset.VertexData + (Subset.Index[i] * Subset.VertexSize));
          glVertex3fv(PGLFloat(Vertex));
        end;
      end;
      var i: Int32;
    begin
      glBegin(GL_TRIANGLES);
      for i := 0 to High(MeshAttach.Mesh.Subsets) do
      begin
        RenderSubset(MeshAttach.Mesh.Subsets[i]);
      end;
      glEnd();
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
      begin
        WeightsOffset := SkinSubset.WeightCount * SizeOf(UInt32);
        for i := 0 to MeshSubset.IndexCount - 1 do
        begin
          Vertex := PUVec3(MeshSubset.VertexData + (MeshSubset.Index[i] * MeshSubset.VertexSize));
          JointIndices := PUInt32Arr(SkinSubset.VertexData + (MeshSubset.Index[i] * SkinSubset.VertexSize));
          JointWeights := PUFloatArr(Pointer(JointIndices) + WeightsOffset);
          VertexSkin := TUVec3.Zero;
          for w := 0 to SkinSubset.WeightCount - 1 do
          begin
            SkinJoint := SkinAttach.JointBindings[JointIndices^[w]];
            S := SkinAttach.Skin.Joints[JointIndices^[w]].Bind * SkinJoint.Transform;
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
  begin
    SetupTransforms(Node.Transform);
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
        Animation.Tracks[i].Target.Transform := Xf;
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
  glClearColor(0.2, 0.2, 0.2, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  //UpdateAnimations(TUFloat(CurTime) * 0.001);
  //PropagateTransforms(Scene.RootNode);
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

end.

