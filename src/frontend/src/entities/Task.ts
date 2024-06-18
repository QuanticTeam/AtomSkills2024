import { SortAndFilterRequest, getSortAndFilterRequestPayload } from '~/shared/api'
import { apiClient } from '~/shared/apiClient'

export interface Task {
  code: string
  title: string
  content: string
  difficulty: number
  time: number
  supplements: string[]
  taskStatuses: TaskProgress[]
}

export const TasksApi = {
  async getAll(
    payload: SortAndFilterRequest & { omCode?: string } = getSortAndFilterRequestPayload(),
  ) {
    const { data } = await apiClient.post<Task[]>('/OM/Tasks', payload)

    return data
  },
  async getOne(code: Task['code']) {
    const { data } = await apiClient.post<Task>('/OM/Task', { code })

    return data
  },
  async begin(payload: { code: Task['code'] }) {
    const { data } = await apiClient.post<TaskProgress>('/Status/TakeTaskInWork', payload)

    return data
  },
  async submit(payload: {
    taskStatusId: number
    fileKeyAndDescriptions: { fileKey: string; description: 'string' }
  }) {
    const { data } = await apiClient.post<TaskProgress>('/Status/SendTaskToCheck', payload)

    return data
  },
  async review(payload: {
    taskId: number
    mark: number
    defects: {
      codes: string[]
      comment: string
      x1: number
      y1: number
      x2: number
      y2: number
    }[]
  }) {
    const { data } = await apiClient.post<TaskProgress>('/Status/VerifiedTask', payload)

    return data
  },
  // What is the result?
  async suggestRetry(payload: {
    taskId: number
    mark: number
    defects: {
      codes: string[]
      comment: string
      x1: number
      y1: number
      x2: number
      y2: number
    }[]
  }) {
    const { data } = await apiClient.post<TaskProgress>('/Status/RecommendedRework', payload)

    return data
  },
  async getTaskProgress(code: Task['code']) {
    const { data } = await apiClient.post<TaskProgress>('/Status/GetTaskStatuses', { code })

    return data
  },
}

export interface TaskProgress {
  id: number
  status: TaskStatusType
  automationSystemStatus: AutomationSystemStatus
  startedAt: string
  finishedAt: string
  mark: number
  fotos: {
    key: string
    comment: string
  }[]

  userKey: string
  taskCode: Task['code']
  recommendations: [
    {
      text: string
      fileKeys: string[]
      taskStatusRecordId: number
    },
  ]
  defects: [
    {
      id: 0
      codes: string[]
      comment: string
      x1: number
      y1: number
      x2: number
      y2: number
      taskStatusRecordId: number
    },
  ]
}

export enum TaskStatusType {
  None = 'None',
  Recommended = 'Recommended',
  InWork = 'InWork',
  SendToCheck = 'SendToCheck',
  AiVerified = 'AiVerified',
  Verified = 'Verified',
}

export enum AutomationSystemStatus {
  None = 'None',
  Complete = 'Complete',
  Error = 'Error',
}
