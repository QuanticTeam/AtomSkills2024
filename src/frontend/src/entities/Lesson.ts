import { SortAndFilterRequest, getSortAndFilterRequestPayload } from '~/shared/api'
import { apiClient } from '~/shared/apiClient'

export interface Lesson {
  code: string
  title: string
  content: string
  traits: string[]
  supplements: string[]
  tasks: string[]
  author: string
}

export const LessonsApi = {
  async getAll(payload: SortAndFilterRequest = getSortAndFilterRequestPayload()) {
    const { data } = await apiClient.post<Lesson[]>('/OM/Lessons', payload)

    return data
  },
  async getOne(code: Lesson['code']) {
    const { data } = await apiClient.post<Lesson>('/OM/Lesson', { code })

    return data
  },
}
