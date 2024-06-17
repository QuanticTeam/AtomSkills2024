import { SortAndFilterRequest, getSortAndFilterRequestPayload } from '~/shared/api'
import { apiClient } from '~/shared/apiClient'

export interface Task {
  code: string
  title: string
  content: string
  difficulty: number
  time: number
  supplements: string[]
  taskStatuses: null
}

export const TasksApi = {
  async getAll(payload: SortAndFilterRequest = getSortAndFilterRequestPayload()) {
    const { data } = await apiClient.post<Task[]>('/OM/Tasks', payload)

    return data
  },
  async getOne(code: Task['code']) {
    const { data } = await apiClient.post<Task>('/OM/Task', { code })

    return data
  },
}
